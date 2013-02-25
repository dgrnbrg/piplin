(ns piplin.z80
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-right bit-shift-left])
  (:use piplin.core))

;http://www.z80.info/decoding.htm

(def uregister
  (enum #{:pc :1 :2 :3 :4 :5 :6 :7 :8}))

(def ^{:doc "uinstr to load into a register"}
  load-uinstr
  (bundle {:src-addr uregister
           :dst-reg uregister}))

(def ^ {:doc "uinstr to store into an address"}
  store-uinstr
  (bundle {:src-reg uregister
           :dst-addr uregister}))

(def ^{:doc "uinstr to load an immediate"}
  imm-uinstr
  (bundle {:dst uregister
           :imm (uintm 16)}))

(def alu-op
  (enum #{:+ :- :shift-left :shift-right-arith
          :shift-right-logical 
          :bit-set :bit-test
          :rotate-left :rotate-right}))

(def ^{:doc "uinstr to do an alu op"}
  alu-uinstr
  (bundle {:src1 uregister
           :src2 uregister
           :op alu-op
           :dst uregister}))

(def microinstruction
  (union {:load load-uinstr
          :store store-uinstr
          :imm imm-uinstr
          :jump uregister
          :alu alu-uinstr}))

(def ^ {:doc "Chooses whether to write to a
             register or to an address."}
  writeback
  (union {:reg (bundle {:reg uregister
                        :data (uintm 16)})
          :mem (bundle {:mem (uintm 16)
                        :data (uintm 16)})
          :nothing nil}))

;This is very simple processor that uses a few registers to execute.
;jumps are accomplished by loading into a register. There is no conditional
;execution yet, but perhaps binary arithmatic + loads into :pc could do it.
;
;There must next be some simple tests of the microcode core to demonstrate it
;works. Then, i'll need to write the decoder state machine and test that it works.
;Finally, i can use a z80 assembler/gameboy assembler to do the next basic and fun
;stuff.

(defn compute-alu
  [])

(defn exec-uinstr
  "Takes a uinstr and a memory, and returns
  a vector of the data and one of a destination
  register or address to write back to."
  [uinstr registers memory]
  (union-match uinstr
    (:load {:keys [src-addr dst-reg]}
      (cast writeback {:reg {:reg dst-reg
                             :data (->> (get registers src-addr)
                                     (get memory))}}))
    (:store {:keys [src-reg dst-addr]}
      (cast writeback {:mem {:mem (get registers src-reg)
                             :data (get registers dst-addr)}}))
    (:imm {:keys [imm dst]}
      (cast writeback {:reg {:reg dst :data imm}}))
    (:alu {:keys [src1 src2 dst op]}
      (let [alu-result (compute-alu src1 src2 op)]
        (cast writeback {:reg {:reg dst :data alu-result}})))))

(comment (defn decode-opcode
  "Takes an opcode and returns the map containing
  `x`, `y`, `z`, `p`, and `q` for use as needed."
  [opcode]
  {:x (bit-slice opcode 6 8)
   :y (bit-slice opcode 3 6)
   :z (bit-slice opcode 0 4)
   :p (bit-slice opcode 4 6)
   :q (bit-slice opcode 3 4)})

(defn table-r
  [x]
  (condp = x
    0 :b
    1 :c
    2 :d
    3 :e
    4 :h
    5 :l
    6 (hl)
    7 :a)) 

(defn table-rp
  [x]
  (condp = x
    0 :bc
    1 :de
    2 :hl
    3 :sp)) 

(defn table-rp2
  [x]
  (condp = x
    0 :bc
    1 :de
    2 :hl
    3 :af)) 

(defn table-cc
  [x]
  (condp = x
    0 :nz
    1 :z
    2 :nc
    3 :c
    4 :po
    5 :pe
    6 :p
    7 :m)) 

(defn table-alu
  [x]
  (condp = x
    0 :add
    1 :adc
    2 :sub
    3 :sbc
    4 :and
    5 :xor
    6 :or
    7 :cp)) 

(defn table-rot
  [x]
  (condp = x
    0 :rlc
    1 :rrc
    2 :rl
    3 :rr
    4 :sla
    5 :sra
    6 :sll
    7 :srl)) 

(defn table-im
  [x]
  (condp = x
    0 :0
    ;1 :0/1
    2 :1
    3 :2
    4 :0
    ;5 :0/1
    6 :1
    7 :2)) 

(defn table-bli
  [a b]
  (condp = [a b]
    [4 0] :ldi
    [5 0] :ldd
    [6 0] :ldir
    [7 0] :lddr
    [4 1] :cpi
    [5 1] :cpd
    [6 1] :cpir
    [7 1] :cpdr
    [4 2] :ini  
    [5 2] :ind
    [6 2] :inir
    [7 2] :indr
    [4 3] :outi  
    [5 3] :outd
    [6 3] :outir
    [7 3] :outdr)) 

(defn decode-unprefixed
  [opcode]
  (let [{:keys [x y z p q]} (decode-opcode opcode)]
    (condp = x 
      0 (condp = z 
          ;relative jumps and assorted ops
          0 (condp = y 
              0 (nop)
              1 (ex-af-af')
              2 (djnz d)
              3 (jr d)
              :else (jr (- y 4) d) ;jump conditional
              )
          ;16 bit load immediate/add
          1 (condp = q 
              0 (ld rp[p] nn)
              1 (add HL, rp[p]))
          ;indirect loading
          2 (condp = q 
              0 (condp = p 
                  0 (ld (BC) A)
                  1 (ld (DE) A)
                  2 (ld (nn) HL)
                  3 (ld (nn) A))
              1 (condp = p 
                  0 (ld (A) BC)
                  1 (ld (A) DE)
                  2 (ld (HL) nn)
                  3 (ld (A) nn)))
          ;16 bit inc/dec
          3 (condp = q 
              0 (inc rp[p])
              1 (dec rp[p]))
          ;8 bit inc
          4 (inc r[y])
          ;8 bit dec
          5 (dec r[y])
          ;8 bit load immediate
          6 (ld r[y] n)
          ;assorted operations on accumulator flags
          7 (condp = y 
              0 (rlca)
              1 (rrca)
              2 (rla)
              3 (rra)
              4 (daa)
              5 (cpl)
              6 (scf)
              7 (ccf)))
      ;TODO: add `and` and `or` for booleans to piplin
      1 (mux2 (and (= z 6)
                   (= y 6))
              ;exception (replaces ld (HL), (HL))
              (halt)
              ;8 bit loading
              (ld r[y], r[z]))
      ;alu operations
      2 (alu[y] r[z])
      3 (condp = z 
          ;conditional return
          0 (ret cc[y])
          ;pop and various ops
          1 (condp = q 
              0 (pop rp2[p])
              1 (condp = p
                  0 (ret)
                  1 (exx)
                  2 (jp hl)
                  3 (ld sp hl)))
          ;conditional jump
          2 (jp cc[y] nn)
          ;assorted operations (IO, interrupts)
          3 (condp = y
              0 (jp nn)
              1 (comment cb prefix)
              2 (out (n) A)
              3 (in A, (n))
              4 (ex (sp) hl)
              5 (ex de hl)
              6 (di)
              7 (ei))
          ;conditional call
          4 (call cc[y] nn)
          ;push and various ops
          5 (condp = q
              0 (push rp2[p])
              1 (condp = p
                  0 (call nn)
                  1 (comment dd prefix)
                  2 (comment ed prefix)
                  3 (comment fd prefix)))
          ;operate on accumulator and immediate
          6 (alu[y] n)
          ;restart
          7 (rst (* y 8 )))))) 

(defn decode-cb-prefix
  "This does bit rotation"
  [opcode]
  (let [{:keys [x y z p q]} (decode-opcode opcode)] 
    (condp = x
      0 (rot [y] r[z])
      1 (bit y, r[z])   
      2 (res y, r[z])   
      3 (set y, r[z])))) 

(defn decode-ed-prefix
  [opcode]
  (let [{:keys [x y z p q]} (decode-opcode opcode)] 
    (condp = x
      0 (nop)
      3 (nop)
      1 (condp = z
          ;input from port with 16 bit addr
          0 (mux2 (= y 6)
                  (in (c))
                  (in r[y], (c)))
          ;output from port with 16 bit addr
          1 (mux2 (= y 6)
                  (out (c), 0)
                  (out (c), r[y]))
          ;16 bit add/sub w/ carry
          2 (mux2 (= q 0)
                  (sbc hl, rp[p])
                  (adc hl, rp[p]))
          ;retrieve/store register pair to/from immediate addr
          3 (mux2 (= q 0)
                  (ld (nn) rp[p])
                  (ld rp[p] (nn)))
          ;negate accumulator
          4 (neg)
          ;return from interrupt
          5 (mux2 (= y 1)
                  (reti)
                  (retn))
          ;set interrupt mode
          6 (im im[y])
          7 (condp = y
              0 (ld i a)
              1 (ld r a)
              2 (ld a i)
              3 (ld a r)
              4 (rrd)
              5 (rld)
              6 (nop)
              7 (nop))
          )
      2 (mux2 (and (<= z 3)
                   (>= y 4))
              (bli y z)
              (nop)))))) 
