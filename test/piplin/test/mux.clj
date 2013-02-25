(ns piplin.test.mux
  (:use clojure.test)
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-right bit-shift-left])
  (:use [piplin types math modules sim mux connect])
  (:use [piplin.types bits boolean enum numbers core-impl binops uintm])
  (:import clojure.lang.ExceptionInfo)) 

(deftest cond-test
  (is (= (cond false 22)
         (clojure.core/cond false 22)))
  (is (= (cond
           (= true true) 0
           false 1)
         (clojure.core/cond
           (= true true) 0
           false 1)))
  (is (= (cond
           (= true true) 0
           :else true)
         (clojure.core/cond
           (= true true) 0
           :else true)))
  (is (thrown-with-msg? ExceptionInfo #"Must include :else.*"
                        (cond
                          (= (uninst true) true) 0
                          false 1)))
  (is (thrown-with-msg? ExceptionInfo #".*different types.*"
                        (cond
                          (= (uninst true) true) 0
                          :else true)))
  (is (= 
        (cond
          true 1
          false 0)  
        (clojure.core/cond
          true 1
          false 0)))
  (is (not=
        1
            (make-sim-fn (cond
                           ;41 != 42
                           (= (uninst 41) 42) ((uintm 3) 2)
                           ;300 > 0
                           (> ((uintm 22) 300) 0) ((uintm 3) 1)
                           :else ((uintm 3) 0)))) "unevaluated sim")
  (is (= 1
         ((make-sim-fn (cond
                         ;41 != 42
                         (= (uninst 41) 42) ((uintm 3) 2)
                         ;300 > 0
                         (> ((uintm 22) 300) 0) ((uintm 3) 1)
                         :else ((uintm 3) 0))))) "evaluted sim"))

(deftest sim-mux2-test
  (let [mod (module [:outputs [flip false]]
                    (connect flip (mux2 flip
                                        false
                                        true)))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 0)
                [:flip])
           false))
    (is (= (get (exec-sim state fns 1)
                [:flip])
           true))
    (is (= (get (exec-sim state fns 2)
                [:flip])
           false))
    (is (= (get (exec-sim state fns 10)
                [:flip])
           false))
    (is (= (get (exec-sim state fns 11)
                [:flip])
           true))))

(deftest mux2-connect-test
  (let [m (module [:outputs [o false
                             i ((uintm 8) 3)]]
                  (mux2 o
                        (do
                          (connect o false)
                          (connect i (+ i i))) 
                        (do
                          (connect o true)
                          (connect i (dec i)))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [:o])
           false))  
    (is (= (get (exec-sim state fns 0)
                [:i])
           ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 1)
                [:o])
           true))
    (is (= (get (exec-sim state fns 1)
                [:i])
           ((uintm 8) 2)))  
    (is (= (get (exec-sim state fns 2)
                [:o])
           false))  
    (is (= (get (exec-sim state fns 2)
                [:i])
           ((uintm 8) 4)))  
    (is (= (get (exec-sim state fns 3)
                [:o])
           true))  
    (is (= (get (exec-sim state fns 3)
                [:i])
           ((uintm 8) 3)))  
    ))

(deftest mux2-connect-test-nested
  (let [e (enum #{:a :b :c}) 
        m (module [:outputs [o false
                             i ((uintm 8) 3)
                             x (e :a)]]
                  (let [lowbit (bit-slice (serialize i) 0 1)]
                    (mux2 o
                          (do
                            (connect o false)
                            (connect i (+ i i))
                            (mux2 (= lowbit ((bits 1) [1]))
                                  (connect x :a)
                                  (connect x :b)))
                          (do
                            (connect o true)
                            (connect x :c)
                            (connect i (dec i))))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [:o])
           false))  
    (is (= (get (exec-sim state fns 0)
                [:i])
           ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 0)
                [:x])
           (e :a)))  
    (is (= (get (exec-sim state fns 1)
                [:o])
           true))
    (is (= (get (exec-sim state fns 1)
                [:i])
           ((uintm 8) 2)))  
    (is (= (get (exec-sim state fns 1)
                [:x])
           (e :c)))  
    (is (= (get (exec-sim state fns 2)
                [:o])
           false))  
    (is (= (get (exec-sim state fns 2)
                [:i])
           ((uintm 8) 4)))  
    (is (= (get (exec-sim state fns 2)
                [:x])
           (e :b)))  
    (is (= (get (exec-sim state fns 3)
                [:o])
           true))  
    (is (= (get (exec-sim state fns 3)
                [:i])
           ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 3)
                [:x])
           (e :c)))))

(deftest cond-connect-test
  (let [e (enum #{:small :medium :big})
        m (module [:outputs [size (e :small)]
                   :feedback [i ((uintm 8) 0)]]
                  (connect i (inc i))
                  (cond
                    (< i 100) (connect size (e :small))
                    (< i 200) (connect size (e :medium))
                    :else (connect size (e :big))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 10)
                [:size])
           (e :small)))  
    (is (= (get (exec-sim state fns 70)
                [:size])
           (e :small)))  
    (is (= (get (exec-sim state fns 110)
                [:size])
           (e :medium)))
    (is (= (get (exec-sim state fns 150)
                [:size])
           (e :medium)))
    (is (= (get (exec-sim state fns 220)
                [:size])
           (e :big)))))

(deftest condp-test
  (let [e (enum #{:a :b :c :d})
        f #(condp = (uninst (e %))
             :a ((uintm 8) 22)
             :b ((uintm 8) 32)
             :c ((uintm 8) 44)
             :d ((uintm 8) 0)
             ((uintm 8) 222))]
    (is (= ((make-sim-fn (f :a))) ((uintm 8) 22)))  
    (is (= ((make-sim-fn (f :b))) ((uintm 8) 32)))  
    (is (= ((make-sim-fn (f :c))) ((uintm 8) 44)))  
    (is (= ((make-sim-fn (f :d))) ((uintm 8) 0)))
    (is (thrown? ExceptionInfo
                 (condp = (uninst (e :a))
                   :a ((uintm 8) 22)
                   :e ((uintm 8) 32)
                   :c ((uintm 8) 44)
                   :d ((uintm 8) 0)))
        "invalid key to enum") 
    (is (thrown? ExceptionInfo
                 (condp = (uninst (e :a))
                   :a ((uintm 8) 22)
                   :b ((uintm 7) 32)
                   :c ((uintm 8) 44)
                   :d ((uintm 8) 0)))
        "not matching bitwidths") 
    ))
