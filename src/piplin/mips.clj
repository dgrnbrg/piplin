(ns piplin.mips
  (:use [slingshot.slingshot])
  (:use [piplin types sim modules])
  (:require [clojure.core :as clj])
  (:require [piplin.math :as h]))

(defn str-to-bits
  [s]
  (let [bit-vec (->> s
                  (filter (partial not= \_))
                  (map
                    #(condp = % \0 0 \1 1
                       (throw (IllegalArgumentException.
                                (str "invalid bit: " %)))))
                  vec)]
    ((h/bits (count bit-vec)) bit-vec)))

(def mips-ops
  {:func "000_000"
   :addi "001_000"
   :addiu "001_001"
   :andi "001_100"
   :beq "000_100" ;todo
   :bgez "000_001" ;todo
   :bgezal "000_001" ;todo
   :bgtz "000_111" ;todo
   :blez "000_110" ;todo
   :bltz "000_001" ;todo
   :bltzal "000_001" ;todo
   :bne "000_101" ;todo
   :j "000_010" ;todo
   :jal "000_011" ;todo
   :lb "100_000" ;todo
   :lui "001_111"
   :lw "100_011" ;todo
   :ori "001_101"
   :sb "101_000" ;todo
   :slti "001_010"
   :sltiu "0010_11"
   :sw "101_011" ;todo
   :xori "001_110"
   })

(def mips-short-funcs
  {:sll "000_000"
   :sllv "000_100"
   :sra "000_011"
   :srl "000_010"
   :syscall "001_100" ;todo
   :xor "100_110"
   })

(def mips-funcs
  {:add "000_0010_0000"
   :addu "000_0010_0001"
   :and "000_0010_0100"
   :div "000_0001_1010" ;todo
   :divu "000_0001_1011" ;todo
   :jr "000_0000_1000"
   :mfhi "000_0001_0000" ;todo
   :mflo "000_0001_0010" ;todo
   :mult "000_0001_1000" ;todo
   :multu "000_0001_1001" ;todo
   :or "000_0010_0101"
   :slt "000_0010_1010"
   :sltu "000_0010_1011"
   :srlv "000_0000_0110"
   :sub "000_0010_0010"
   :subu "000_0010_0011"
   })

(def mips-branches
  {:bgez "00001"
   :bgezal "10001"
   :bltzal "10000"})

(def reg (h/enum #{:0 :1 :2 :3
                   :4 :5 :6 :7
                   :8 :9 :10 :11
                   :12 :13 :14 :15
                   :16 :17 :18 :19
                   :20 :21 :22 :23
                   :24 :25 :26 :27
                   :28 :29 :30 :31}))

(def u32m (h/uintm 32))

(def reg-or-imm (h/union {:reg reg :imm u32m}))

(def alu-op (h/enum #{:add
                    :addu
                    :sub
                    :and
                    :lui
                    :or
                    :xor
                    :slt
                    :sltu
                    :sll
                    }))

(def alu-unresolved-cmd
  (h/bundle {:op alu-op
           :x reg-or-imm
           :y reg-or-imm
           :dst reg})) 

(def alu-cmd (h/bundle {:op alu-op
                      :x u32m
                      :y u32m
                      :dst reg}))

(def wb-result (h/bundle {:data u32m
                        :dst reg}))

(comment
  MIPS architecture decomposed
  
  given an instruction, determine if it's an
  alu op (arithmetic and branching), a load op,
  a store op, or an immediate jump (can be
  bypassed). Returns a union of the possibilities.
  ALU ops' arguments are unions of immediate
  and register. Destination is a register.
  others are undetermined.
  
  given 2 immediate_or_registers, return
  a pair of immediates, resolving the registers.
  
  given 2 immediates, an alu op, and the current pc,
  return the next pc, an immediate, and a register
  to write to
  
  given a register an an immediate, store the immediate
  in the register)

(defn compare-key-via-str
  [m k b]
  (h/= (str-to-bits (get m k)) b))

(defn zext32
  "zero extend bits to 32"
  [b]
  (h/deserialize u32m
    (h/bit-cat
      (h/cast (h/bits (- 32 (h/bit-width-of (typeof b)))) 0)
      b)))

(defn decode-func
  "Takes a partially decoded function and completes
  the decoding"
  [func short-func rs rt rd sa]
  (let [cmd {:x {:reg rs}
             :y {:reg rt}
             :dst rd}]
    (->>
      (h/condp (partial
                 compare-key-via-str
                 mips-short-funcs) short-func
        :sll (-> cmd
               (assoc-in
                 [:op] :sll)
               (assoc-in
                 [ :x]
                 {:imm (zext32 sa)}))
        :sllv (assoc-in
                cmd [ :op] :sll)
        :sra (-> cmd
               (assoc-in
                 [ :op] :sra)
               (assoc-in
                 [ :x]
                 {:imm (zext32 sa)}))
        :srl (-> cmd
               (assoc-in
                 [ :op] :srl)
               (assoc-in
                 [ :y]
                 {:imm (zext32 sa)}))
        :xor (assoc-in
               cmd [ :op] :xor)
        (assoc-in
          cmd
          [:op]
          (h/condp (partial
                     compare-key-via-str mips-funcs) func
            :add :add
            :addu :addu
            :and :and
            :jr :jr
            :or :or
            :slt :slt
            :sltu :sltu
            :srl :srlv
            :sub :sub
            :subu :subu)))
      (h/cast alu-unresolved-cmd)
      identity
      )))

(defn decode
  "Takes an instruction and returns an
  alu-cmd or mem-cmd"
  [inst]
  (when-not (= (typeof inst) (h/bits 32))
    (throw (IllegalArgumentException. "must be bits32")))
  (->>
    (let [b (partial h/bit-slice inst)
          op (b 26 32)
          rs (h/deserialize reg (b 21 26)) 
          rt (h/deserialize reg (b 16 21)) 
          imm (zext32 (b 0 16)) 
          target (b 0 26)
          rd (h/deserialize reg (b 11 16)) 
          sa (b 6 11)
          short-func (b 0 6)
          func (b 0 11)
          partial-imm {:x {:reg rs}
                       :y {:imm imm}
                       :dst rt}
          ]
      (h/mux2 (compare-key-via-str mips-ops op :func)
        (decode-func func short-func
                     rs rt rd sa)
        (h/cast alu-unresolved-cmd
                (assoc partial-imm :op
                       (h/condp (partial compare-key-via-str mips-ops) op
                         :addi :add
                         :addiu :addu
                         :andi :and
                         :lui :lui
                         :ori :or
                         :slti :slt
                         :sltiu :sltu
                         :xori :xor
                         ;branches
                         ;jumps
                         ;loads
                         ;stores
                         )))))
    (h/cast alu-unresolved-cmd)
    ))

(defn alu
  "Takes an alu command and returns
  a writeback result"
  [cmd]
  (let [{:keys [op x y dst]} cmd]
    (->>
      {:data (h/condp op h/=
               :add (h/+ x y)
               :sub (h/- x y)) 
       :dst dst}
      (cast wb-result) 
      )))

(comment (clojure.java.shell/sh "gcc"
                       "-xc"
                       "-nostdlib"
                       "-c"
                       "-o"
                       "tmp.o"
                       "-"
                       :in (.getBytes
                             "int main() {
                               return 22;
                             }")))

;addiu encoded
;(decode (str-to-bits "001001_00001_00010_1000_0000_0000_0000"))
