(ns piplin.test.sints 
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or])
  (:use [piplin.types bundle sints bits boolean core-impl binops])
  (:use [piplin types mux modules sim connect protocols])
  (:import clojure.lang.ExceptionInfo) 
  (:use clojure.test))

(deftest sints-basics
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
                ((sints 8) 100))))
  )

(deftest sints-compare-cases
  (is (> ((sints 8) 100) 0))
  (is (> ((sints 8) 100) -120))
  (is (< ((sints 8) -100) 120)))
