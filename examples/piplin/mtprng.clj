(ns piplin.mtprng
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp or and])
  (:use [piplin core [protocols :only [typeof value]]] [swiss-arrows.core :only [-<>]]
        [piplin.util :only [let']]
        ))

(def N 624)
(def M 397)
(def MATRIX_A 0x9908b0df)
(def UPPER_MASK 0x80000000)
(def LOWER_MASK 0x7fffffff)

(defn zero-bits
  [n]
    (if (= n 1)
      #b0
      (bit-cat #b0 (zero-bits (dec n)))))

(defn bit-shift-right
  [x n]
  (let [t (typeof x)]
    (-<> x
         (serialize)
         (bit-slice <> n (bit-width-of t))
         (bit-cat (zero-bits n) <>)
         (deserialize t <>))))

(defn bit-shift-left
  [x n]
  (let [t (typeof x)]
    (-<> x
         (serialize)
         (bit-slice <> 0 (- (bit-width-of t) n))
         (bit-cat <> (zero-bits n))
         (deserialize t <>))))

(defn mt_init
  "Initialize Mersenne Twister"
  [seed]
  (vec (reductions
    (fn [prev index]
      (let [top-two-bits (bit-slice (serialize prev) 30 32)
            bottom-two-bits (bit-slice (serialize prev) 0 2)
            xor-two-bits (bit-xor top-two-bits bottom-two-bits)
            prev' (bit-cat (bit-slice (serialize prev) 2 32) xor-two-bits)
            prev'uint (deserialize (uintm 32) prev')]

        (+ (* 1812433253 prev'uint) index)))
    ((uintm 32) seed)
    (range 1 N))))

(defn mt_gen_single
  "Generate next Mersenne Twister number."
  [mt mt-index]
  (let' [y (bit-or (bit-and (get mt mt-index) UPPER_MASK)
                  (bit-and (get mt (mux2 (= (dec N) mt-index)
                                         (cast (typeof mt-index) 0)
                                         (inc mt-index)))
                           LOWER_MASK))
        mt-index-mid (mux2 (< mt-index (- N M))
                           (+ mt-index M)
                           (+ (- mt-index N) M))
        y>>1 (bit-shift-right y 1)
        mt-update (bit-xor (get mt mt-index-mid) y>>1 (mux2 (= #b1 (bit-slice (serialize y) 0 1)) ((uintm 32) MATRIX_A) ((uintm 32) 0)))
        mt-index-update (mux2 (= mt-index (dec N)) (cast (typeof mt-index) 0) (inc mt-index))
        rand mt-update
        rand (bit-xor rand (bit-shift-right rand 11))
        rand (bit-xor rand (bit-and (bit-shift-left rand 7) 0x9d2c5680))
        rand (bit-xor rand (bit-and (bit-shift-left rand 15) 0xefc60000))
        rand (bit-xor rand (bit-shift-right rand 18))]
    [mt-update mt-index-update rand]))

;(mt_gen_single (mt_init 0x12341234) 0)

(doseq [x (->>
  (iterate
  (fn [[mt mt-index]]
    (let [[mt-update mt-index-update rand] (mt_gen_single mt mt-index)]
      [(assoc mt mt-index mt-update) mt-index-update rand]))
  [(mt_init 0x12341234) 0])
  (drop 1)
  (map
    (fn [[_ _ r]]
      (value r)))
  (take 10))] (printf "%d\n" x))

(defmodule mersenne-twister
  [seed]
  [:outputs [rand ((uintm 32) 0)
             valid false]
   :feedback [mt (cast (array (uintm 32) N) (mt_init seed))
              mt-index ((uintm 10) 0)]]
  (let [[mt-update mt-index-update rand'] (mt_gen_single mt mt-index)]
    (connect mt-index mt-index-update)
    (connect rand rand')
    (connect (get mt mt-index) mt-update)))

(spit "mod.v" (modules->all-in-one (mersenne-twister 0x12341233)))
(spit "mod.v" (modules->verilog+testbench (mersenne-twister 0x12341233) 100))
;(with-out-str (pprint (modules->all-in-one (mersenne-twister 0x12341233))))
;(spit "blah" (with-out-str (pprint (mersenne-twister 0x12341233))))
