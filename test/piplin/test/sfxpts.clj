(ns piplin.test.sfxpts
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right])
  (:use [piplin.types bundle sfxpts bits boolean core-impl binops uintm])
  (:use [piplin types mux modules sim connect protocols verilog])
  (:import clojure.lang.ExceptionInfo)
  (:use clojure.test
        plumbing.core
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

(let [t (sfxpts 4 4)]
  (def sfxpts-mul
    (modulize
      {:x (fnk [x] (inc x))
       :y (fnk [x]
               (let [x-sfxpts (->> x
                                serialize
                                (deserialize t))]
                 (* x-sfxpts x-sfxpts)))}
      {:x ((uintm 8) 0)
       :y (cast t 0.0)})))

(deftest sfxpts-mul-verilog
  (icarus-test (verify sfxpts-mul 500)))

(let [t (sfxpts 12 16)]
  (defn sfxpts-quadratic
    [coeff1 coeff2 coeff3]
    (modulize
      {:x (fnk [x] (inc x))
       :y (fnk [x]
               (let [x' (deserialize t (serialize x))
                     poly #_(* x' x') (+ (* coeff1 x' x')
                                         (* coeff2 x')
                                         coeff3)]
                 poly))}
      {:x ((uintm (bit-width-of t)) 0)
       :y (cast t 0.0)})))

(deftest sfxpts-synthesis
  (icarus-test (verify (sfxpts-quadratic 2.2 0.1 -0.8) 10000)))
