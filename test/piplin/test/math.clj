(ns piplin.test.math
  (:use clojure.test)
  (:import slingshot.ExceptionInfo)
  (:use [slingshot.slingshot :only [throw+]])
  (:use [piplin types math modules sim]))

(deftest j-long-test
  (is (= 3 (+ 1 2)))
  (is (= 6 (* 2 3)))
  (is (thrown? ExceptionInfo (promote :j-long "lx"))))

(deftest uintm-test
  (letfn [(um8 [v] (instance (uintm 8) v))]
    (is (= (+ (um8 200) 1) (um8 201)))
    (is (= (+ (um8 200) 60) (um8 4)))
    (is (= (+ 1 2 3 (um8 4)) (um8 10)))
    (is (= (instance (uintm 8) -1 :constrain) (um8 255)))
    (is (= (instance (uintm 8) 256 :constrain) (um8 0)))
    (is (thrown? ExceptionInfo (promote (uintm 3) "lx")))
    (is (thrown? ExceptionInfo (promote (uintm 3) -1)))
    (is (thrown? ExceptionInfo (promote (uintm 3) 100)))
    (is (= (- 0 (um8 1)) (um8 255)))))

(deftest binop-error-test
  (let [e (error "hi")]
    (is (= (try-errors (+ 0 (throw+ e))) [e]))
    (is (= (try-errors (+ 0 1 2 3 (throw+ e))) [e]))
    (is (= (try-errors (+ 0 1 (throw+ e) 3 (throw+ e))) [e e]))
    (is (= (try-errors (+ (throw+ e) 0)) [e]))
    (is (= (try-errors (+ (throw+ e) (throw+ e))) [e e]))))

(deftest sim-uintm-bits-test
  (let [mod (module [:outputs [c (instance (uintm 8) 0)
                               d (instance (bits 8) 0)]]
                    (connect c (+ c 1))
                    (connect d (slice (get-bits c) 0 4)))
        sim (make-sim mod)
        init-state (first sim)
        init-fns (second sim)]
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [(:token mod) :c])
           (instance (uintm 8) 10))
        "ran and counted up to 10")
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [(:token mod) :d])
           (instance (bits 4) [1 0 0 1]))
        "ran and counted up to 10 bits")
    (is (= (get (exec-sim init-state
                          init-fns
                          18)
                [(:token mod) :d])
           (instance (bits 4) [0 0 0 1]))
        "ran and counted up to 10 bits")))
