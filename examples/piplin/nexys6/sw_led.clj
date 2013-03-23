(ns piplin.nexys6.sw-led
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-left bit-shift-right])
  (:use [piplin.types.union :only [get-tag get-value]]
        piplin.core)
  (:use plumbing.core)
  (:require [plumbing.graph :as graph])
  (:use [piplin.vga :only [xvga]])
  (:use [piplin.seven-segment-decoder :only [decoder]]))

(def seven-seg-map
  {:top 0
   :upper-left 5
   :lower-left 4
   :bottom 3
   :lower-right 2
   :upper-right 1
   :middle 6})

(defn complex-under-mag?
  "returns true if the magnitude of `complex`
  is less than `mag`"
  [complex mag]
  (let [r (real-part complex)
        i (imag-part complex)
        mag**2 (+ (* r r) (* i i))]
    (< mag**2 (* mag mag))))

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

(def inst
  (union
    {:jump (uintm 8)
     :reg->acc (uintm 3)
     :acc->reg (uintm 3)
     :set (bundle {:reg1 (uintm 3) :reg2 (uintm 3)})
     :loadi (bundle {:imm (sfxpts 3 13)
                     :reg (uintm 3)})
     :add (uintm 3)
     :sub (uintm 3)
     :mul (uintm 3)
     :noop (bits 1)}))

(defn ->inst
  [op val]
  (cast inst {op val}))

(def program
  (ifelse-memory
    {0 (->inst :loadi {:imm 1.0 :reg 1})
     1 (->inst :loadi {:imm 1.0 :reg 2})
     2 (->inst :loadi {:imm 1.4 :reg 3})
     3 (->inst :loadi {:imm 1.0 :reg 4})
     4 (->inst :loadi {:imm 0.3 :reg 5})
     5 (->inst :reg->acc 1)
     6 (->inst :mul 1)
     7 (->inst :mul 3)
     8 (->inst :acc->reg 6)
     9 (->inst :reg->acc 2)
     10 (->inst :add 4)
     11 (->inst :sub 6)
     12 (->inst :acc->reg 6)
     13 (->inst :reg->acc 1)
     14 (->inst :mul 5)
     15 (->inst :acc->reg 2)
     16 (->inst :reg->acc 6)
     17 (->inst :acc->reg 1)
     18 (->inst :set {:reg1 1 :reg2 2})
     19 (->inst :jump 5)
     :else (->inst :noop #b0)

     }))

(def program2
  (ifelse-memory
    {0 (->inst :loadi {:imm -2.0 :reg 1})
     1 (->inst :loadi {:imm 0.05 :reg 2})
     2 (->inst :loadi {:imm -0.005 :reg 3})
     3 (->inst :loadi {:imm -2.0 :reg 4})
     4 (->inst :loadi {:imm 0.01 :reg 5})
     5 (->inst :set {:reg1 1 :reg2 4})
     6 (->inst :reg->acc 1)
     7 (->inst :add 2)
     8 (->inst :acc->reg 1)
     9 (->inst :reg->acc 2)
     10 (->inst :add 3)
     11 (->inst :acc->reg 2)
     12 (->inst :reg->acc 4)
     13 (->inst :add 5)
     14 (->inst :acc->reg 4)
     15 (->inst :jump 5)
     :else (->inst :noop #b0)}))

(defmodule simplecpu
  []
  [:outputs [
             memaddr ((uintm 14) 0)
             memdata #b1
             pc ((uintm 8) 0)
             fp-accum (cast (sfxpts 3 13) 0.0)
             ]
   :feedback [
              fp-regs (cast (array (sfxpts 3 13) 8)
                            (repeat 8 0.0))]]
  (let [inst (program2 pc)]
    (connect pc (mux2 (= :jump (get-tag inst))
                      (get-value :jump inst)
                      (inc pc)))
    (union-match inst
      (:add reg
            (connect fp-accum (+ fp-accum (get fp-regs reg))))
      (:sub reg
            (connect fp-accum (- fp-accum (get fp-regs reg))))
      (:mul reg
            (connect fp-accum (* fp-accum (get fp-regs reg))))
      (:reg->acc reg
                 (connect fp-accum (get fp-regs reg)))
      (:noop _
            (connect fp-accum fp-accum)))

    (let [tag (get-tag inst)
          {loadi-reg :reg loadi-imm :imm} (get-value :loadi inst)
          acc->reg-reg (get-value :acc->reg inst)
          data (condp = tag
                 :loadi loadi-imm
                 :acc->reg fp-accum
                 (cast (sfxpts 3 13) 0.0))
          addr (condp = tag
                 :loadi loadi-reg
                 :acc->reg acc->reg-reg
                 ((uintm 3) 0))]
      (connect (get fp-regs addr) data))
    (connect memaddr
             (mux2 (= (get-tag inst) :set)
                   (let [{:keys [reg1 reg2]} (get-value :set inst)
                         x (get fp-regs reg1)
                         y (get fp-regs reg2)
                         x' (serialize x)
                         y' (serialize y)]
                     (deserialize
                       (uintm 14)
                       (bit-cat
                         (->> (bit-slice x' 9 16)
                           (deserialize (uintm 7))
                           (+ 63)
                           serialize)
                         (->> (bit-slice y' 9 16)
                           (deserialize (uintm 7))
                           (+ 63)
                           serialize))))
                   memaddr))))

(let [num-type (sfxpts 4 14)
      c-type (complex num-type num-type)]
  (defmodule julia-set
  []
  [:inputs [x num-type
            y num-type]
   :outputs [ready? false
             color #b0]
   :feedback [v (cast c-type [0.0 0.0])
              loops ((uintm 5) 0)]]
  (let [v' (+ (* v v)
              (cast c-type [0.8 0.156]))]
    (connect v (mux2 ready?
                     (deserialize
                       c-type
                       (bit-cat (serialize x) (serialize y)))
                     v'))
    (connect ready?
             (or (>= loops 50)
                 (complex-under-mag? v 2.0)))
    (connect loops (mux2 ready? 0 (inc loops)))
    (connect color (bit-slice (serialize loops) 0 1)))))

(defmodule led->switch []
  [:outputs [led #b0000_0000
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]
   :feedback [timer ((uintm 24) 1)
              counter ((uintm 16) 0)
              faster-counter ((uintm 14) 0)
              buffer-index ((uintm 14) 0)
              video-buffer (cast (array (bits 1)
                                        (int (Math/pow 2 14)))
                                 (take (int (Math/pow 2 14))
                                       (interleave (repeat #b0)
                                                   (repeat #b1))))]
   :modules [deco (decoder 16 seven-seg-map)
             vga (xvga)
             cpu (simplecpu)]]
  (connect Hsync vga$hsync)
  (connect Vsync vga$vsync)
  (connect (get video-buffer cpu$memaddr)
           #b1 #_cpu$memdata)
  (let [hcount (serialize vga$hcount)
        border? (or (= vga$hcount 0)
                    (= vga$hcount 1023)
                    (= vga$vcount 0)
                    (= vga$vcount 767))
        r (condp = (bit-slice sw 0 2)
            #b00 (bit-slice hcount 8 9)
            #b01 (mux2 border? #b1 #b0)
            ;else, do buffered image
            (get video-buffer (bit-cat
                                (bit-slice (serialize vga$vcount) 1 8)
                                (bit-slice (serialize vga$hcount) 1 8)))

            )
        g (condp =  (bit-slice sw 0 2)
            #b00 (bit-slice hcount 7 8)
            #b01 (mux2 border? #b1 #b0)
            ;else, do buffered image
            (get video-buffer (bit-cat
                                (bit-slice (serialize vga$vcount) 1 8)
                                (bit-slice (serialize vga$hcount) 1 8)))
            )
        b (condp = (bit-slice sw 0 2)
            #b00 (bit-slice hcount 6 7)
            #b01 (mux2 border? #b1 #b0)
            ;else, do buffered image
            (get video-buffer (bit-cat
                                (bit-slice (serialize vga$vcount) 1 8)
                                (bit-slice (serialize vga$hcount) 1 8)))
            )]
    (connect vgaRed (bit-cat r r r))
    (connect vgaGreen (bit-cat g g g))
    (connect vgaBlue (bit-cat b b)))
  (connect led sw)
  (connect timer (inc timer))
  (connect counter (mux2 (= 0 timer)
                         (inc counter)
                         counter))
  (connect faster-counter (inc faster-counter))
  #_(connect faster-counter (mux2 (= 0 (deserialize
                                       (uintm 20)
                                       (bit-slice (serialize timer) 0 20)))
                                (inc faster-counter)
                                faster-counter))
  (connect deco$in
           (bit-cat #b00 cpu$memaddr)
           #_(bit-cat (bit-slice (serialize cpu$fp-accum) 8 16) cpu$pc)
           #_(serialize counter))
  (let [
        ;Must multiplex in the millisecond regime
        current-state (bit-slice
                        (serialize timer) 10 12)
        anode (condp = current-state
                #b00 #b1110
                #b01 #b1101
                #b10 #b1011
                ;#b11
                #b0111)
        cathode (condp = current-state
                  #b00 (bit-slice
                         deco$out 0 7)
                  #b01 (bit-slice
                         deco$out 7 14)
                  #b10 (bit-slice
                         deco$out 14 21)
                 ; #b11
                  (bit-slice
                         deco$out 21 28))]
    (connect seven_seg_anode anode)
    (connect seven_seg_cathode
             ;bit-not => active-low
             ;extra bit disables the decimal
             (bit-cat #b1 (bit-not cathode)))))

#_(spit "tmp" (with-out-str (led->switch)))
#_(spit "nexys-leds.v" (modules->all-in-one (led->switch)))

(def adder
  {:sum (fnk adder [x y] (+ x y))
   :difference (fnk addr [x y] (- x y))})

#_(pprint ((graph/eager-compile (assoc adder
                            :x (fnk [] (piplin.types/uninst ((uintm 8) 2)))
                            :y (fnk [] (piplin.types/uninst ((uintm 8) 3))))) {}))



(declare fetch store)
(def fetch get)
(def store assoc)

(def adder
  {:sum (fnk [x y] (+ x y))})

(compile-root (modulize adder nil) :x ((uintm 8) 3) :y 4)
(-> (compile-root (modulize :root adder nil) :x ((uintm 8) 3) :y 4)
  (->verilog {[:root :sum] "sum"})
  println
  )

(def counter
  {:value (fnk [value] (inc value))})

(-> (compile-root (modulize :root counter {:value ((uintm 8) 0)}))
  (->verilog {[:root :value] "val"})
  print)
(-> (compile-root (modulize counter {:value ((uintm 8) 0)}))
  (sim 10)
  pprint)

(def quadratic-counter
  {:value (fnk [value]
               (let [counter ((modulize counter
                                        {:value
                                         ((uintm 8) 0)}))]
                 (+ value (:value counter))))})

(-> (compile-root (modulize :root quadratic-counter {:value ((uintm 8) 0)}))
  (->verilog {[:root :value] "val"})
  println)

(-> (compile-root
      (modulize quadratic-counter {:value ((uintm 8) 0)}))
  (sim 10)
  pprint )

(def adder {:+1 (fnk [input] (inc input))})

(println (->verilog (compile-root (modulize adder nil) :input (input "in" (uintm 8))) {}))

(def double-adder
  {:input (fnk [input]
            (let [adder1 ((modulize adder nil)
                            :input input)
                  adder2 ((modulize adder nil)
                            :input (:+1 adder1))]
              (:+1 adder2)))})
(-> (compile-root (modulize :root double-adder {:input ((uintm 8) 3)}))
  (->verilog {[:root :input] "double_count"})
  (println)
;  (sim 10)
;  (pprint)
  )

(def alu
  {:alu-adder (fnk [x y]
                   (let [{:keys [sum]} ((modulize [adder] nil)
                                          :x x
                                          :y y)]
                     sum))
   :result (fnk [dest x y op memory regfile imm]
                (let [x (fetch regfile x)
                      y (fetch regfile y)]
                  (assoc
                    (condp = op
                      :add {:val (+ x y) :valid? true}
                      :sub {:val (- x y) :valid? true}
                      :mul {:val (* x y) :valid? true}
                      :load {:val (fetch memory x) :valid? true}
                      :imm {:val imm :value? true}
                      :else {:val x :valid? false})
                    :dest dest)))
   :branch (fnk [jump-addr op x pc regfile]
                (cond (and (= op :jz) (zero? (fetch regfile x)))
                      jump-addr
                      (= op :jump)
                      jump-addr
                      :else
                      (inc pc)))
   :memory-wb (fnk [x addr op memory regfile]
                   (store memory (= op :store) (fetch regfile addr) (fetch regfile x)))})

(def inst
  (union
    {:jump (uintm 8)
     :jz (bundle {:reg (uintm 4) :addr (uintm 8)})
     :store (bundle {:addr (uintm 3) :reg (uintm 4)})
     :load (bundle {:addr (uintm 3) :reg (uintm 4) :dest (uintm 4)})
     :add (bundle {:x (uintm 4) :y (uintm 4) :dest (uintm 4)})
     :sub (bundle {:x (uintm 4) :y (uintm 4) :dest (uintm 4)})
     :mul (bundle {:x (uintm 4) :y (uintm 4) :dest (uintm 4)})
     :imm (bundle {:dest (uintm 4) :imm (uintm 8)})}))

(def fetch-decode
  {:inst (fnk [pc program]
              (fetch program pc))
   :x (fnk [inst]
           (union-match inst
             (:jz {:keys [reg]} reg)
             (:jump _ 0)
             (:store {:keys [reg]} reg)
             (:load {:keys [reg]} reg)
             (:add {:keys [x]} x)
             (:sub {:keys [x]} x)
             (:mul {:keys [x]} x)
             (:imm _ 0)))
   :y (fnk [inst]
           (union-match inst
             (:jz _ 0)
             (:jump _ 0)
             (:store _ 0)
             (:load _ 0)
             (:add {:keys [y]} y)
             (:sub {:keys [y]} y)
             (:mul {:keys [y]} y)
             (:imm _ 0)))
   :jump-addr (fnk [inst op]
                   (mux2 (= op :jump)
                     (get-value :jump inst)
                     (:addr (get-value :jz inst))))
   :op (fnk [inst]
            (get-tag inst))
   :dest (fnk [inst]
           (union-match inst
             (:jz _ 0)
             (:jump _ 0)
             (:store _ 0)
             (:load {:keys [dest]} dest)
             (:add {:keys [dest]} dest)
             (:sub {:keys [dest]} dest)
             (:mul {:keys [dest]} dest)
             (:imm {:keys [dest]} dest)))
   :imm (fnk [inst]
             (:imm (get-value :imm inst)))
   :pc (fnk [branch]
            branch)
   :addr (fnk [branch]
              (:addr (get-value :store inst)))})

(def writeback
  {:regfile-wb (fnk [result regfile]
                    (let [{:keys [dest val valid?]} result]
                      (store regfile valid? dest val)))})

(defn make-processor
  []
  ((modulize
     ;seq of included parts
     (merge fetch-decode alu writeback)
     ;Elements which are actually registers
     {:pc (uintm 8)
      :regfile (array (uintm 8) 8)
      :memory (array (uintm 8) 256)})
     :program
     {0 (cast inst {:jump 0})}
     ))

;;  With this info, modulize does the following:
;;  1. makes a set of registers out of the given parts
;;  2. computes the dependency graph of everything with the
;;     explicitly called out elements as the roots
;;  3. Look over every graph node. Each register should be named
;;     once, which makes the assignment to that register. This is
;;     enforced across all given maps. Look over the other nodes:
;;     some of them will return `store` invocations, which generate
;;     write ports on the array memories. `store` can also return the
;;     updated array, and it's up to the user to know what their code
;;     will generate.
;;  4. Now, the graphs can be merged and evaluated, with the inputs
;;     being the old values of the registers, and the outputs being
;;     the way to update registers and memories.
;;
;;  Thoughts:
;;  - get-tag and get-value are useful even though they're unsafe--that's
;;  inherent in dynamism, due to the wasted circuit
;;  - modulize returns a function that takes keyword args and returns a map
;;  of outputs. Whenever you call moduleize, you can check the global *current-module*
;;  to know whether you're being created under which module. When created as a
;;  non-root module, the modulize call will have to generate stubs for its input
;;  and output and add a copy of itself to the module's chain, or it will have
;;  to add its nested storage elements to the overall storage pool, munge all
;;  its internal signal names, and "lift" its structure into its parent.
