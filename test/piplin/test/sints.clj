(ns piplin.test.sints
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right])
  (:import clojure.lang.ExceptionInfo)
  (:use clojure.test
        piplin.core
        plumbing.core
        [piplin.types :only [instance]]
        piplin.test.util))

(deftest sints-basics
  (is (= ((sints 8) 0) ((sints 8) 0)))
  (is (= (cast (sints 8) ((sints 8) 0)) ((sints 8) 0)))
  (is (= 127 ((sints 8) 127)))
  (is (= 127 (instance (sints 8) 200 :constrain)))
  (is (= 100 ((sints 8) 100)))
  (is (= -100 ((sints 8) -100)))
  (is (= -128 ((sints 8) -128)))
  (is (= -128 (instance (sints 8) -200 :constrain))))

(deftest sints-math-bounds
  (let [valid-range (into #{} (map (sints 4) (range -8 8)))]
   (doseq [x (range -8 8) y (range -8 8)
           :let [x ((sints 4) x)
                 y ((sints 4) y)]]
    (is (valid-range (- x y)))
    (is (valid-range (* x y)))
    (is (valid-range (+ x y))))))

(deftest sints-math-cases
  (is (= 5 (+ ((sints 8) 10)
              ((sints 8) -5))))
  (is (= 5 (- ((sints 8) 10)
              ((sints 8) 5))))
  (is (= -5 (+ ((sints 8) -10)
               ((sints 8) 5))))
  (is (= -5 (- ((sints 8) -10)
               ((sints 8) -5))))
  (is (= -2 (* ((sints 8) -1)
               ((sints 8) 2))))
  (is (= -2 (* ((sints 8) 1)
               ((sints 8) -2))))
  (is (= 2 (* ((sints 8) -1)
               ((sints 8) -2))))
  (is (= 127 (+ ((sints 8) 100)
                ((sints 8) 100)))))

(deftest sints-compare-cases
  (is (> ((sints 8) 100) 0))
  (is (> ((sints 8) 100) -120))
  (is (< ((sints 8) -100) 120)))

(deftest sints-bits-roundtrip
  (doseq [x (map (sints 4) (range -8 8))]
    (is (= x (deserialize (sints 4) (serialize x))))))

(defn sints-adder
  [n]
  (modulize
    {:sum (fnk [x y] (+ x y))
     :x-max? (fnk [x] (= x (piplin.types.sints/max-value (sints n))))
     :x (fnk [x-max? x]
             (mux2 x-max?
                   (piplin.types.sints/min-value (sints n))
                   (inc x)))
     :y (fnk [x-max? y]
             (mux2 x-max?
                   (inc y)
                   y))}
    {:sum ((sints n) 0)
     :x (piplin.types.sints/min-value (sints n))
     :y (piplin.types.sints/min-value (sints n))}))

(deftest sints-adder-verilog
  (icarus-test (verify
                 (sints-adder 4) (* 16 16 2))))

(defn sints-subtractor
  [n]
  (modulize {:difference (fnk [x y] (- x y))
             :x-max? (fnk [x] (= x (piplin.types.sints/max-value (sints n))))
             :x (fnk [x-max? x]
                     (mux2 x-max?
                           (piplin.types.sints/min-value (sints n))
                           (inc x)))
             :y (fnk [x-max? y]
                     (mux2 x-max?
                           (inc y)
                           y))}
            {:difference ((sints n) 0)
             :x (piplin.types.sints/min-value (sints n))
             :y (piplin.types.sints/min-value (sints n))}))

(deftest sints-subtractor-verilog
  (icarus-test (verify
                 (sints-subtractor 4) (* 16 16 2))))

(defn sints-multiplier
  [n]
  (modulize {:prod (fnk [x y] (* x y))
             :x-max? (fnk [x] (= x (piplin.types.sints/max-value (sints n))))
             :x (fnk [x-max? x]
                     (mux2 x-max?
                           (piplin.types.sints/min-value (sints n))
                           (inc x)))
             :y (fnk [x-max? y]
                     (mux2 x-max?
                           (inc y)
                           y))}
            {:prod ((sints n) 0)
             :x (piplin.types.sints/min-value (sints n))
             :y (piplin.types.sints/min-value (sints n))}))

(deftest sints-multiplier-verilog
  (icarus-test (verify
                 (sints-multiplier 6) (* 64 64 2))))

(defn sints-extender
  [n m]
  (modulize
    {:x (fnk [x] (inc x))
     :y (fnk [x] (sign-extend m x))}
    {:x (piplin.types.sints/min-value (sints n))
     :y (piplin.types.sints/min-value (sints m))} ))

(deftest sints-extender-verilog
  (icarus-test (verify
                 (sints-extender 4 8) 100))
  (icarus-test (verify
                 (sints-extender 4 5) 100))
  (icarus-test (verify
                 (sints-extender 4 4) 100)))
