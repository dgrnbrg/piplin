(ns piplin.mips
  (:refer-clojure :exclude [cast condp cond = not=])
  (:use [slingshot.slingshot])
  (:use [piplin types sim modules mux protocols])
  (:use [piplin.types binops uintm enum bundle union bits])
  (:require [clojure.core :as clj])
  (:require [piplin.types.core-impl :as h]))


(def mips-ops
  (enum {:func #b000_000
         :addi #b001_000
         :addiu #b001_001
         :andi #b001_100
         :beq #b000_100 ;todo
         :bgez #b000_001 ;todo
         :bgezal #b000_001 ;todo
         :bgtz #b000_111 ;todo
         :blez #b000_110 ;todo
         :bltz #b000_001 ;todo
         :bltzal #b000_001 ;todo
         :bne #b000_101 ;todo
         :j #b000_010 ;todo
         :jal #b000_011 ;todo
         :lb #b100_000 ;todo
         :lui #b001_111
         :lw #b100_011 ;todo
         :ori #b001_101
         :sb #b101_000 ;todo
         :slti #b001_010
         :sltiu #b0010_11
         :sw #b101_011 ;todo
         :xori #b001_110
         } :allow-dups))

(def mips-short-funcs
  (enum {:sll #b000_000
         :sllv #b000_100
         :sra #b000_011
         :srl #b000_010
         :syscall #b001_100 ;todo
         :xor #b100_110
         }))

(def mips-funcs
  (enum {:add #b000_0010_0000
         :addu #b000_0010_0001
         :and #b000_0010_0100
         :div #b000_0001_1010 ;todo
         :divu #b000_0001_1011 ;todo
         :jr #b000_0000_1000
         :mfhi #b000_0001_0000 ;todo
         :mflo #b000_0001_0010 ;todo
         :mult #b000_0001_1000 ;todo
         :multu #b000_0001_1001 ;todo
         :or #b000_0010_0101
         :slt #b000_0010_1010
         :sltu #b000_0010_1011
         :srlv #b000_0000_0110
         :sub #b000_0010_0010
         :subu #b000_0010_0011
         }))

(def mips-branches
  (enum {:bgez #b00001
         :bgezal #b10001
         :bltzal #b10000}))

(def reg (enum #{:0 :1 :2 :3
                 :4 :5 :6 :7
                 :8 :9 :10 :11
                 :12 :13 :14 :15
                 :16 :17 :18 :19
                 :20 :21 :22 :23
                 :24 :25 :26 :27
                 :28 :29 :30 :31}))

(def u32m (uintm 32))

(def reg-or-imm (union {:reg reg :imm u32m}))

(def alu-op (enum #{:add
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
  (bundle {:op alu-op
           :x reg-or-imm
           :y reg-or-imm
           :dst reg}))

(def alu-cmd (bundle {:op alu-op
                      :x u32m
                      :y u32m
                      :dst reg}))

(def wb-result (bundle {:data u32m
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

(defn zext32
  "zero extend bits to 32"
  [b]
  (deserialize u32m
    (bit-cat
      (cast (bits (- 32 (bit-width-of (typeof b)))) 0)
      b)))

(defn decode-func
  "Takes a partially decoded function and completes
  the decoding"
  [func short-func rs rt rd sa]
  (let [cmd {:x {:reg rs}
             :y {:reg rt}
             :dst rd}]
    (->>
      (condp = (deserialize mips-short-funcs short-func)
        :sll (-> cmd
               (assoc
                 :op :sll)
               (assoc
                 :x
                 {:imm (zext32 sa)}))
        :sllv (assoc
                cmd :op :sll)
        :sra (-> cmd
               (assoc
                 :op :sra)
               (assoc
                 :x
                 {:imm (zext32 sa)}))
        :srl (-> cmd
               (assoc
                 :op :srl)
               (assoc
                 :y
                 {:imm (zext32 sa)}))
        :xor (assoc
               cmd [ :op] :xor)
        (assoc
          cmd
          :op
          (condp = (deserialize mips-funcs func)
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
      (cast alu-unresolved-cmd)
      identity
      )))

(defn decode
  "Takes an instruction and returns an
  alu-cmd or mem-cmd"
  [inst]
  (when-not (= (typeof inst) (bits 32))
    (throw (IllegalArgumentException. "must be bits32")))
  (->>
    (let [b (partial bit-slice inst)
          op (b 26 32)
          rs (deserialize reg (b 21 26))
          rt (deserialize reg (b 16 21))
          imm (zext32 (b 0 16))
          target (b 0 26)
          rd (deserialize reg (b 11 16))
          sa (b 6 11)
          short-func (b 0 6)
          func (b 0 11)
          partial-imm {:x {:reg rs}
                       :y {:imm imm}
                       :dst rt}
          ]
      (mux2 (= (deserialize mips-ops op) :func)
            (decode-func func short-func
                         rs rt rd sa)
            (cast alu-unresolved-cmd
                  (assoc partial-imm :op
                         (condp = (deserialize mips-ops op)
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
    (cast alu-unresolved-cmd)
    ))

(defn alu
  "Takes an alu command and returns
  a writeback result"
  [cmd]
  (let [{:keys [op x y dst]} cmd]
    (->>
      {:data (condp op =
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
