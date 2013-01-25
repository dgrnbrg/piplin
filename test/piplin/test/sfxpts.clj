(ns piplin.test.sfxpts
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or])
  (:use [piplin.types bundle sfxpts bits boolean core-impl binops uintm])
  (:use [piplin types mux modules sim connect protocols [verilog :only [modules->verilog+testbench]]])
  (:import clojure.lang.ExceptionInfo) 
  (:use clojure.test
        piplin.test.util))

(deftest sfxpts-basics
  (is (= (value (cast (sfxpts 3 5) 2.25)) (+ (bit-shift-left 2 5) 8)))
  (is (= (value (cast (sfxpts 8 8) 4.0)) (bit-shift-left 4 8)))
  (is (= (* (cast (sfxpts 8 8) 37.075) 3.0) (* 37.075 3)))
  (is (not= (* (cast (sfxpts 8 8) 0.345845) 3.0)
            (* 0.345845 3)))
  (is (= (+ (cast (sfxpts 8 0) 127.0) 9.0) 127.0))
  (is (= (- (cast (sfxpts 8 0) -120.0) 50.0) -128.0))
  (is (= 15 (bit-width-of (sfxpts 8 7))))
  (is (= 15 (bit-width-of (sfxpts 7 8))))
  (let [x (cast (sfxpts 8 8) 2.66)]
    (is (= x (->> x
               serialize
               (deserialize (typeof x)))))))

(let [t (sfxpts 12 16)]
  (defmodule sfxpts-quadratic
    [coeff1 coeff2 coeff3]
    [:feedback [x ((uintm (bit-width-of t)) 0)
                y (cast t 0.0)]]
    (connect x (inc x))
    (let [x' (deserialize t (serialize x))
          poly #_(* x' x') (+ (* coeff1 x' x')
                  (* coeff2 x')
                  coeff3)]
      (connect y poly))))

(deftest sfxpts-synthesis
  (icarus-test (modules->verilog+testbench
                 (sfxpts-quadratic 2.2 0.1 -0.8)
                 10000)))

;NOTE: i don't think this is necessarily possible...
;(x*( 2 - a x* ))
#_(deftest invsq-test
  (letfn [(newton-method
            [x0 x]
             (let [x* (* x (- 2 (* x0 x)))]
               (println x* x (= x* x))
               (if (= x* x)
                     x*
                     #_(cast (typeof x0) 0)
                     (newton-method x0 x*))))]
    (println (newton-method (cast (sfxpts 8 8) 2) (cast (sfxpts 8 8) 2)))
(println (float (/ -32768 (Math/pow 2 8))))
    )
  )
