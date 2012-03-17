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
  (h/bit-cat
    (h/cast (h/bits (- 32 (h/bit-width-of (typeof b)))) 0)
    b))

(defn decode-func
  "Takes a partially decoded function and completes
  the decoding"
  [func short-func rs rt rd sa]
  (let [cmd {:alu {:x {:reg rs}
                   :y {:reg rt}
                   :dst rd}}]
    (->>
      (h/condp (partial
                 compare-key-via-str
                 mips-short-funcs) short-func
        :sll (-> cmd
               (assoc-in
                 [:alu :op] :sll)
               (assoc-in
                 [:alu :x]
                 {:imm (zext32 sa)}))
        :sllv (assoc-in
                cmd [:alu :op] :sll)
        :sra (-> cmd
               (assoc-in
                 [:alu :op] :sra)
               (assoc-in
                 [:alu :x]
                 {:imm (zext32 sa)}))
        :srl (-> cmd
               (assoc-in
                 [:alu :op] :srl)
               (assoc-in
                 [:alu :y]
                 {:imm (zext32 sa)}))
        :xor (assoc-in
               cmd [:alu :op] :xor)
        (h/condp (partial
                   compare-key-via-str mips-funcs) func
          :add (assoc-in
                 cmd [:alu :op] 
                 :add)
          :addu (assoc-in
                  cmd [:alu :op] 
                  :addu)
          :and (assoc-in
                 cmd [:alu :op] 
                 :and)
          :jr (assoc-in
                cmd [:alu :op] 
                :jr)
          :or (assoc-in
                cmd [:alu :op] 
                :or)
          :slt (assoc-in
                 cmd [:alu :op] 
                 :slt)
          :sltu (assoc-in
                  cmd [:alu :op] 
                  :sltu)
          :srl (assoc-in
                 cmd [:alu :op] 
                 :srlv)
          :sub (assoc-in
                 cmd [:alu :op] 
                 :sub)
          :subu   (assoc-in
                    cmd [:alu :op] 
                    :subu)))
      :alu
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
          imm (h/deserialize (h/uintm 32)
                             (zext32 (b 0 16))) 
          target (b 0 26)
          rd (h/deserialize reg (b 11 16)) 
          sa (b 6 11)
          short-func (b 0 6)
          func (b 0 11)
          partial-imm {:alu {:x {:reg rs}
                             :y {:imm imm}
                             :dst rt}}
          ]
      (h/condp (partial compare-key-via-str mips-ops) op
        :addi (assoc-in partial-imm
                        [:alu :op] :add)
        :addiu (assoc-in partial-imm
                         [:alu :op] :addu)
        :andi (assoc-in partial-imm
                        [:alu :op] :and)
        :lui (assoc-in partial-imm
                       [:alu :op] :lui)
        :ori (assoc-in partial-imm
                       [:alu :op] :or)
        :slti (assoc-in partial-imm
                        [:alu :op] :slt)
        :sltiu (assoc-in partial-imm
                         [:alu :op] :sltu)
        :xori (assoc-in partial-imm
                        [:alu :op] :xor)
        :func (decode-func func short-func
                           rs rt rd sa)
        ;branches
        ;jumps
        ;loads
        ;stores
        ))
    identity
    :alu
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
