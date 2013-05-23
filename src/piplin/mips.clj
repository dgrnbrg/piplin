(ns piplin.mips
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-right bit-shift-left pos? neg? zero?])
  (:use plumbing.core)
  (:use piplin.core))

(defmacro defunion
  [name & clauses]
  `(def ~name (union (hash-map ~@clauses))))

(defmacro defenum
  [name & values]
  `(def ~name (enum (conj #{} ~@values))))

;; Register index
;; TODO indices are wrong for the decoder
(def rindx (uintm 5))
(def $zero (rindx 0))
(def $at (rindx 1))
(def $v0 (rindx 2))
(def $v1 (rindx 3))
(def $a0 (rindx 4))
(def $a1 (rindx 5))
(def $a2 (rindx 6))
(def $a3 (rindx 7))
(def $t0 (rindx 8))
(def $t1 (rindx 9))
(def $t2 (rindx 10))
(def $t3 (rindx 11))
(def $t4 (rindx 12))
(def $t5 (rindx 13))
(def $t6 (rindx 14))
(def $t7 (rindx 15))
(def $t8 (rindx 24))
(def $t9 (rindx 25))
(def $s0 (rindx 16))
(def $s1 (rindx 17))
(def $s2 (rindx 18))
(def $s3 (rindx 19))
(def $s4 (rindx 20))
(def $s5 (rindx 21))
(def $s6 (rindx 22))
(def $s7 (rindx 23))
(def $s8 (rindx 30))
(def $k0 (rindx 26))
(def $k1 (rindx 27))
(def $gp (rindx 28))
(def $sp (rindx 29))
(def $ra (rindx 31))

(def simm (bits 16))
(def zimm (bits 16))
(def shamt (bits 5))
(def target (bits 26))
(def cp0indx (bits 5))
(def data (bits 32))
(def addr (uintm 32))

(defunion instr

  ;; Memory operations
  :lw (bundle {:rbase rindx :rdst rindx :offset simm})
  :sw (bundle {:rbase rindx :rsrc rindx :offset simm})

  ;; Arithmetic operations
  :addiu (bundle {:rsrc rindx :rdst rindx :imm simm})
  :slti  (bundle {:rsrc rindx :rdst rindx :imm simm})
  :sltiu (bundle {:rsrc rindx :rdst rindx :imm zimm})
  :andi  (bundle {:rsrc rindx :rdst rindx :imm zimm})
  :ori   (bundle {:rsrc rindx :rdst rindx :imm zimm})
  :xori  (bundle {:rsrc rindx :rdst rindx :imm zimm})
  :lui   (bundle {            :rdst rindx :imm zimm})

  :sll   (bundle {:rsrc rindx :rdst rindx :shamt shamt})
  :srl   (bundle {:rsrc rindx :rdst rindx :shamt shamt})
  :sra   (bundle {:rsrc rindx :rdst rindx :shamt shamt})
  :sllv  (bundle {:rsrc rindx :rdst rindx :rshamt rindx})
  :srlv  (bundle {:rsrc rindx :rdst rindx :rshamt rindx})
  :srav  (bundle {:rsrc rindx :rdst rindx :rshamt rindx})
  :addu  (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :subu  (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :and   (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :or    (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :xor   (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :nor   (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :slt   (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})
  :sltu  (bundle {:rsrc1 rindx :rsrc2 rindx :rdst rindx})

  ;; Jump and branch operations
  :j     (bundle {:target target})
  :jal   (bundle {:target target})
  :jr    (bundle {:rsrc rindx})
  :jalr  (bundle {:rsrc rindx :rdst rindx})
  :beq   (bundle {:rsrc1 rindx :rsrc2 rindx :offset simm})
  :bne   (bundle {:rsrc1 rindx :rsrc2 rindx :offset simm})
  :blez  (bundle {:rsrc rindx :offset simm})
  :bgtz  (bundle {:rsrc rindx :offset simm})
  :bltz  (bundle {:rsrc rindx :offset simm})
  :bgez  (bundle {:rsrc rindx :offset simm})

  :mfc0  (bundle {:rdst rindx :cop0src cp0indx})
  :mtc0  (bundle {:rsrc rindx :cop0dst cp0indx})

  :illegal (bits 1)
  )

(defn- ->instr
  "Takes a nested map that has the same structure
   as an instr but with `bits` in every location instead
   of the correct type. Recursively converts all the
   bits to the correct types, then casts that result
   to `instr`."
  [instr-data]
  (let [[[tag value]] (seq instr-data)
        schema (:schema (get (:schema instr) tag))
        data (if (= tag :illegal)
               value
               (reduce (fn [x [k v]]
                         (assoc x k (deserialize
                                      v (get value k))))
                       {}
                       schema))]
    (cast instr {tag data})))

(let [opFUNC  #b000000  fcSLL   #b000000
      opRT    #b000001  fcSRL   #b000010
      opRS    #b010000  fcSRA   #b000011
      fcSLLV  #b000100
      opLW    #b100011  fcSRLV  #b000110
      opSW    #b101011  fcSRAV  #b000111
      fcADDU  #b100001
      opADDIU #b001001  fcSUBU  #b100011
      opSLTI  #b001010  fcAND   #b100100
      opSLTIU #b001011  fcOR    #b100101
      opANDI  #b001100  fcXOR   #b100110
      opORI   #b001101  fcNOR   #b100111
      opXORI  #b001110  fcSLT   #b101010
      opLUI   #b001111  fcSLTU  #b101011

      opJ     #b000010
      opJAL   #b000011
      fcJR    #b001000
      fcJALR  #b001001
      opBEQ   #b000100
      opBNE   #b000101
      opBLEZ  #b000110
      opBGTZ  #b000111
      rtBLTZ  #b00000
      rtBGEZ  #b00001

      rsMFC0  #b00000
      rsMTC0  #b00100
      
      bit-cat (fn [& args] (apply bit-cat (map serialize args)))]
  (defn encode
    [instr]
    (union-match instr

      (:lw {:keys [rbase rdst offset]}
           (bit-cat opLW rbase rdst offset))
      (:sw {:keys [rbase rsrc offset]}
           (bit-cat opSW rbase rsrc offset))

      (:addiu {:keys [rsrc rdst imm]}
              (bit-cat opADDIU rsrc rdst imm))
      (:slti {:keys [rsrc rdst imm]}
             (bit-cat opSLTI, rsrc, rdst, imm))
      (:sltiu {:keys [rsrc rdst imm]}
              (bit-cat opSLTIU, rsrc, rdst, imm))
      (:andi {:keys [rsrc rdst imm]}
             (bit-cat opANDI, rsrc, rdst, imm))
      (:ori {:keys [rsrc rdst imm]}
            (bit-cat opORI, rsrc, rdst, imm))
      (:xori {:keys [rsrc rdst imm]}
             (bit-cat opXORI, rsrc, rdst, imm))
      (:lui {:keys [rdst imm]}
            (bit-cat opLUI, #b00_000, rdst, imm))

      (:sll {:keys [rsrc rdst shamt]}
            (bit-cat opFUNC, #b00_000, rsrc, rdst, shamt, fcSLL))
      (:srl {:keys [rsrc rdst shamt]}
            (bit-cat opFUNC, #b00_000, rsrc, rdst, shamt, fcSRL))
      (:sra {:keys [rsrc rdst shamt]}
            (bit-cat opFUNC, #b00_000, rsrc, rdst, shamt, fcSRA))

      (:sllv {:keys [rsrc rdst rshamt]}
             (bit-cat opFUNC, rshamt, rsrc, rdst, #b00_000, fcSLLV))
      (:srlv {:keys [rsrc rdst rshamt]}
             (bit-cat opFUNC, rshamt, rsrc, rdst, #b00_000, fcSRLV))
      (:srav {:keys [rsrc rdst rshamt]}
             (bit-cat opFUNC, rshamt, rsrc, rdst, #b00_000, fcSRAV))

      (:addu {:keys [rsrc1 rsrc2 rdst]}
             (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcADDU))
      (:subu {:keys [rsrc1 rsrc2 rdst]}
             (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcSUBU))
      (:and {:keys [rsrc1 rsrc2 rdst]}
            (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcAND)) 
      (:or {:keys [rsrc1 rsrc2 rdst]}
           (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcOR)) 
      (:xor {:keys [rsrc1 rsrc2 rdst]}
            (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcXOR)) 
      (:nor {:keys [rsrc1 rsrc2 rdst]}
            (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcNOR)) 
      (:slt {:keys [rsrc1 rsrc2 rdst]}
            (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcSLT)) 
      (:sltu {:keys [rsrc1 rsrc2 rdst]}
             (bit-cat opFUNC, rsrc1, rsrc2, rdst, #b00_000, fcSLTU)) 

      (:j {:keys [target]}
          (bit-cat opJ, target))
      (:jal {:keys [target]}
            (bit-cat opJAL, target))
      (:jr {:keys [rsrc]}
           (bit-cat opFUNC, rsrc, #b00_000, #b00_000, #b00_000, fcJR))
      (:jalr {:keys [rsrc rdst]}
             (bit-cat opFUNC, rsrc, #b00_000, rdst, #b00_000, fcJALR))
      (:beq {:keys [rsrc1 rsrc2 offset]}
            (bit-cat opBEQ, rsrc1, rsrc2, offset))
      (:bne {:keys [rsrc1 rsrc2 offset]}
            (bit-cat opBNE, rsrc1, rsrc2, offset))
      (:blez {:keys [rsrc offset]}
             (bit-cat opBLEZ, rsrc, #b00_000, offset))
      (:bgtz {:keys [rsrc offset]}
             (bit-cat opBGTZ, rsrc, #b00_000, offset))
      (:bltz {:keys [rsrc offset]}
             (bit-cat opRT, rsrc, rtBLTZ, offset))
      (:bgez {:keys [rsrc offset]}
             (bit-cat opRT, rsrc, rtBGEZ, offset))

      (:mfc0 {:keys [rdst cop0src]}
             (bit-cat opRS, rsMFC0, rdst, cop0src, #b000_000_000_00))
      (:mtc0 {:keys [rsrc cop0dst]}
             (bit-cat opRS, rsMTC0, rsrc, cop0dst, #b000_000_000_00))
      )
    )

  (defn decode
    [instr-bits]
    (let [opcode (bit-slice instr-bits 26 32)
          rs (bit-slice instr-bits 21 26)
          rt (bit-slice instr-bits 16 21)
          rd (bit-slice instr-bits 11 16)
          shamt (bit-slice instr-bits 6 11)
          funct (bit-slice instr-bits 0 6)
          imm (bit-slice instr-bits 0 16)
          target (bit-slice instr-bits 0 26)]

      (condp = opcode
        opLW (->instr {:lw {:rbase rs :rdst rt :offset imm}})
        opSW (->instr {:sw {:rbase rs :rsrc rt :offset imm}})
        opADDIU (->instr {:addiu { :rsrc rs :rdst rt :imm imm}})
        opSLTI (->instr {:slti { :rsrc rs :rdst rt :imm imm}})
        opSLTIU (->instr {:sltiu { :rsrc rs :rdst rt :imm imm}})
        opANDI (->instr {:andi { :rsrc rs :rdst rt :imm imm}})
        opORI (->instr {:ori { :rsrc rs :rdst rt :imm imm}})
        opXORI (->instr {:xori { :rsrc rs :rdst rt :imm imm}})
        opLUI (->instr {:lui {  :rdst rt :imm imm}})
        opJ (->instr {:j { :target target}})
        opJAL (->instr {:jal { :target target}})
        opBEQ (->instr {:beq { :rsrc1 rs :rsrc2 rt :offset imm}})
        opBNE (->instr {:bne { :rsrc1 rs :rsrc2 rt :offset imm}})
        opBLEZ (->instr {:blez { :rsrc rs :offset imm}})
        opBGTZ (->instr {:bgtz { :rsrc rs :offset imm}})
        opFUNC  
        (condp = funct
          fcSLL   (->instr {:sll   {:rsrc rt  :rdst rd  :shamt shamt }})
          fcSRL   (->instr {:srl   {:rsrc rt  :rdst rd  :shamt shamt }})
          fcSRA   (->instr {:sra   {:rsrc rt  :rdst rd  :shamt shamt }})
          fcSLLV  (->instr {:sllv  {:rsrc rt  :rdst rd  :rshamt rs   }})
          fcSRLV  (->instr {:srlv  {:rsrc rt  :rdst rd  :rshamt rs   }})
          fcSRAV  (->instr {:srav  {:rsrc rt  :rdst rd  :rshamt rs   }})
          fcADDU  (->instr {:addu  {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcSUBU  (->instr {:subu  {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcAND   (->instr {:and   {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcOR    (->instr {:or    {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcXOR   (->instr {:xor   {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcNOR   (->instr {:nor   {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcSLT   (->instr {:slt   {:rsrc1 rs :rsrc2 rt :rdst rd     } })
          fcSLTU  (->instr {:sltu  {:rsrc1 rs :rsrc2 rt :rdst rd     }})
          fcJR    (->instr {:jr    {:rsrc rs                         }})
          fcJALR  (->instr {:jalr  {:rsrc rs  :rdst rd               }})
          (->instr {:illegal #b0}))

        opRT
        (condp = rt
          rtBLTZ (->instr {:bltz {:rsrc rs :offset imm}})
          rtBGEZ (->instr {:bltz {:rsrc rs :offset imm}})
          (->instr {:illegal #b0}))

        opRS
        (condp = rs
          rsMFC0 (->instr {:mfc0 {:rdst rt :cop0src rd}})
          rsMTC0 (->instr {:mtc0 {:rsrc rt :cop0dst rd}})
          (->instr {:illegal #b0}))

        (->instr {:illegal #b0})))))

(defn sext32
  "Sign extends the input to 32 bits"
  [x]
  (let [width (bit-width-of (typeof x))
        high-bit (bit-slice x (dec width) width)]
    (if (not= width 32)
      (sext32 (bit-cat high-bit x))
      x)))

(defn zext32
  "Zero extends the input to 32 bits"
  [x]
  (let [width (bit-width-of (typeof x))]
    (if (not= width 32)
      (zext32 (bit-cat #b0 x))
      x)))

(defmacro defbarrelshifter
  [name docstring [x shamt] case-fn ovf-expr]
  `(defn ~name
     ~docstring
     [~x ~shamt]
     (condp = ~shamt
       ~@(mapcat (fn [i]
                   [`(cast (bits 32) ~i) (list case-fn i)])
                (range 32))
       ~ovf-expr
       )))

(defbarrelshifter sra
  "Shift right arithmetic"
  [x shamt]
  (fn [i]
    (let [high-bit (bit-slice x 31 32)]
      (apply bit-cat (concat (repeat i high-bit)
                             [(bit-slice x i 32)]))))
  (apply bit-cat (repeat 32 (bit-slice x 31 32)))
  )

(defbarrelshifter srl
  "Shift right logical"
  [x shamt]
  (fn [i]
      (apply bit-cat (concat (repeat i #b0)
                             [(bit-slice x i 32)])))
  (cast (bits 32) 0))

(defbarrelshifter sll
  "Shift left logical"
  [x shamt]
  (fn [i]
      (apply bit-cat (concat 
                             [(bit-slice x 0 (- 32 i))]
                       (repeat i #b0) 
                       )))
  (cast (bits 32) 0))

(defn s>
  [x y]
  (> (deserialize (sints 32) x) (deserialize (sints 32) y)))
(defn s>=
  [x y]
  (>= (deserialize (sints 32) x) (deserialize (sints 32) y))) 
(defn s<
  [x y]
  (< (deserialize (sints 32) x) (deserialize (sints 32) y))) 
(defn s<=
  [x y]
  (<= (deserialize (sints 32) x) (deserialize (sints 32) y)))

(defenum alu-op-types
  :lw
  :add
  :sub
  :slt
  :sltu
  :and
  :or
  :xor
  :nor
  :sll
  :srl
  :sra
  :jal
  :beq
  :bne
  :blez
  :bgtz
  :bltz
  :bgez)

(def alu-op
  (bundle {:op alu-op-types
           :pc addr
           :imm simm
           :x data
           :y data
           :dst rindx}))

(def store-op
  (bundle {:pc addr
           :addr data
           :data data}))

(defunion alu-or-store-op
  :alu alu-op
  :store store-op)

(defn- ->alu-op
  "Returns an alu-or-store-op"
  [op pc x y dst]
  (cast alu-or-store-op
        {:alu {:op op :pc pc :x x :y x :dst dst :imm (cast simm 0)}}))

(defn- ->br-op
  "Returns an alu-or-store-op"
  [op pc offset x y]
  (cast alu-or-store-op
        {:alu {:op op :pc pc :x x :y x :dst (cast rindx 0) :imm offset}}))

(defn- ->store-op
  "Returns an alu-or-store-op"
  [pc addr data]
  (cast alu-or-store-op
        {:store {:pc pc :addr addr :data data}}))

(defn resolve-operands
  "Takes a decoded instr, the pc, and the regfile
   and resolves the operands, returning either
   a store or an alu op."
  [instr pc regfile]
  (union-match
    instr
    ;; Memory operations
    (:lw {:keys [rbase rdst offset]}
         (->alu-op :lw pc (get regfile rbase) (zext32 offset) rdst))
    (:sw {:keys [rbase rsrc offset]}
         (->store-op pc
                     (serialize (+ (cast (uintm 32) (get regfile rbase))
                                   (cast (uintm 32) (zext32 offset))))
                     (get regfile rsrc)))

    (:addiu {:keys [rsrc rdst imm]}
            (->alu-op :add pc (get regfile rsrc) (sext32 imm) rdst))
    (:slti {:keys [rsrc rdst imm]}
           (->alu-op :slt pc (get regfile rsrc) (sext32 imm) rdst))
    (:sltiu {:keys [rsrc rdst imm]}
            (->alu-op :sltu pc (get regfile rsrc) (zext32 imm) rdst))
    (:andi {:keys [rsrc rdst imm]}
           (->alu-op :and pc (get regfile rsrc) (zext32 imm) rdst))
    (:ori {:keys [rsrc rdst imm]}
          (->alu-op :or pc (get regfile rsrc) (zext32 imm) rdst))
    (:xori {:keys [rsrc rdst imm]}
           (->alu-op :xor pc (get regfile rsrc) (zext32 imm) rdst))
    (:lui {:keys [rdst imm]}
          (->alu-op :add pc
                    (bit-cat imm (cast (bits 16) 0))
                    (cast data 0)
                    rdst))

    (:sll {:keys [rsrc rdst shamt]}
          (->alu-op :sll pc (get regfile rsrc) (zext32 shamt) rdst))
    (:srl {:keys [rsrc rdst shamt]}
          (->alu-op :srl pc (get regfile rsrc) (zext32 shamt) rdst)) 
    (:sra {:keys [rsrc rdst shamt]}
          (->alu-op :sra pc (get regfile rsrc) (zext32 shamt) rdst)) 
    (:sllv {:keys [rsrc rdst rshamt]}
           (->alu-op :sll pc (get regfile rsrc) (get regfile rshamt) rdst)) 
    (:srlv {:keys [rsrc rdst rshamt]}
           (->alu-op :srl pc (get regfile rsrc) (get regfile rshamt) rdst)) 
    (:srav {:keys [rsrc rdst rshamt]}
           (->alu-op :sra pc (get regfile rsrc) (get regfile rshamt) rdst)) 
    (:addu {:keys [rsrc1 rsrc2 rdst]}
           (->alu-op :add pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 
    (:subu {:keys [rsrc1 rsrc2 rdst]}
           (->alu-op :sub pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 
    (:and {:keys [rsrc1 rsrc2 rdst]}
          (->alu-op :and pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 
    (:or  {:keys [rsrc1 rsrc2 rdst]}
         (->alu-op :or pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 
    (:xor {:keys [rsrc1 rsrc2 rdst]}
          (->alu-op :xor pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 
    (:nor {:keys [rsrc1 rsrc2 rdst]}
          (->alu-op :nor pc (get regfile rsrc1) (get regfile rsrc2) rdst))
    (:slt {:keys [rsrc1 rsrc2 rdst]}
          (->alu-op :slt pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 
    (:sltu {:keys [rsrc1 rsrc2 rdst]}
           (->alu-op :sltu pc (get regfile rsrc1) (get regfile rsrc2) rdst)) 

    ;; Jump and branch operations
    (:j {:keys [target]}
        (->alu-op :jal pc (zext32 target) (cast data 0) $zero))
    (:jal {:keys [target]}
          (->alu-op :jal pc (zext32 target) (cast data 0) $ra))
    (:jr {:keys [rsrc]}
         (->alu-op :jal pc (get regfile rsrc) (cast data 0) $zero))
    (:jalr {:keys [rsrc rdst]}
           (->alu-op :jal pc (get regfile rsrc) (cast data 0) $ra))
    (:beq {:keys [rsrc1 rsrc2 offset]}
          (->br-op :beq pc (get regfile rsrc1) (get regfile rsrc2) offset))
    (:bne {:keys [rsrc1 rsrc2 offset]}
          (->br-op :bne pc (get regfile rsrc1) (get regfile rsrc2) offset))
    (:blez {:keys [rsrc offset]}
           (->br-op :blez pc (get regfile rsrc) (cast data 0) offset))
    (:bgtz {:keys [rsrc offset]}
           (->br-op :bgtz pc (get regfile rsrc) (cast data 0) offset))
    (:bltz {:keys [rsrc offset]}
           (->br-op :bltz pc (get regfile rsrc) (cast data 0) offset))
    (:bgez {:keys [rsrc offset]}
           (->br-op :bgez pc (get regfile rsrc) (cast data 0) offset))

    (:mfc0 {:keys [rdst cop0src]}
           ;; TODO implement this
           (->alu-op :add pc (cast data 0) (cast data 0) (cast rindx 0)))
    (:mtc0 {:keys [rsrc cop0dst]}
           ;; TODO implement this
           (->alu-op :add pc (cast data 0) (cast data 0) (cast rindx 0)))
    (:illegal _
              (->alu-op :add pc (cast data 0) (cast data 0) (cast rindx 0)))))

(def reg-writeback
  (bundle {:pc addr
           :dst rindx
           :val data}))

(defn ->writeback
  ""
  [pc dst val]
  ;(println "pc =" pc "dst =" dst "val =" val)
  (cast reg-writeback {:pc pc :dst dst :val val}))

(defn execute
  ""
  [op memory]
  (union-match
    op
    (:store {pc :pc} (->writeback (+ pc 4) $zero 0))
    (:alu {:keys [op pc x y dst imm]}
          (let [pc+4 (+ pc 4)
                pc+imm (+ pc (cast (uintm 32) (sext32 imm)))
                ux (deserialize (uintm 32) x)
                uy (deserialize (uintm 32) y)
                x+y (serialize (+ ux uy))
                x-y (serialize (- ux uy))]
            (condp = op
              :lw (->writeback pc+4 dst (get memory x+y))
              :add (->writeback pc+4 dst x+y)
              :sub (->writeback pc+4 dst x-y)
              :slt (->writeback pc+4 dst (mux2 (s< x y)
                                               (cast (bits 32) 0)
                                               (cast (bits 32) 1)))
              :sltu (->writeback pc+4 dst (mux2 (< ux uy)
                                               (cast (bits 32) 0)
                                               (cast (bits 32) 1)))
              :and (->writeback pc+4 dst (bit-and x y))
              :or (->writeback pc+4 dst (bit-or x y))
              :xor (->writeback pc+4 dst (bit-xor x y))
              :nor (->writeback pc+4 dst (bit-not (bit-or x y)))
              :sll (->writeback pc+4 dst (sll x y)) 
              :srl (->writeback pc+4 dst (srl x y)) 
              :sra (->writeback pc+4 dst (sra x y)) 
              
              :jal (->writeback x dst (serialize pc+4))
              :beq (->writeback (mux2 (= x y)
                                      pc+4
                                      pc+imm) $zero 0)
              :bne (->writeback (mux2 (not= x y)
                                      pc+4
                                      pc+imm) $zero 0)
              :blez (->writeback (mux2 (s<= x y)
                                      pc+4
                                      pc+imm) $zero 0)
              :bgtz (->writeback (mux2 (s>= x y)
                                      pc+4
                                      pc+imm) $zero 0)
              :bltz (->writeback (mux2 (s< x y)
                                      pc+4
                                      pc+imm) $zero 0)
              :bgez (->writeback (mux2 (s> x y)
                                      pc+4
                                      pc+imm) $zero 0)
              (->writeback pc+4 $zero 22))))))

(defn ifelse-memory
  "Returns a synthesizable function which takes an
  address and returns the value associated with that
  key. Requires an `:else` key in the map for any unknown
  slots. This key cannot be left out."
  ([data]
   (ifelse-memory (dissoc data :else) (:else data)))
  ([data elseval]
   (if (seq data)
     (let [[[k v] & more] (seq data)]
       (fn [addr]
         (mux2 (= k addr)
               v
               ((ifelse-memory more elseval) addr))))
     (fn [addr] elseval))))

(decode (piplin.types/uninst #b001001_00001_00010_1000_0000_0000_0000))
(decode #b001001_00001_00010_1000_0000_0000_0000)

(def mipsv1
  (modulize
    :mips
    {
     :pc (fnk [alu-result]
              (:pc alu-result))
     :instr-bits (fnk [pc]
                      ((ifelse-memory
                         {:else (cast (bits 32) 0)
                          (cast (uintm 32) 0)
                          #b001001_00001_00010_1000_0000_0000_0000}) pc))
     :decoded-instr (fnk [instr-bits]
                         (decode instr-bits))
     :resolved-instr (fnk [decoded-instr pc regfile]
                          (resolve-operands decoded-instr pc regfile))
     :alu-result (fnk [resolved-instr memory]
                      (execute resolved-instr memory))
    ; :store-result (fnk [resolved-instr memory]
    ;                    (store memory
    ;                           (= :store
    ;                              (piplin.types.union/get-tag
    ;                                resolved-instr))
    ;                           (-> (piplin.types.union/get-value
    ;                                 :store
    ;                                 resolved-instr)
    ;                               :addr)
    ;                           (-> (piplin.types.union/get-value
    ;                                 :store
    ;                                 resolved-instr)
    ;                               :data)))
    ; :register-writback (fnk [alu-result regfile]
    ;                         (let [{:keys [dst val]} alu-result]
    ;                           (store regfile
    ;                                  (not= $zero dst)
    ;                                  dst
    ;                                  val)))
     
     }
    {:pc (cast addr 0)
     :regfile (cast (array data 32) (repeat 32 (cast data 0)))
     :memory (cast (array data 100000) (repeat 100000 (cast data 0)))}))

;(sim (compile-root mipsv1) 0)
