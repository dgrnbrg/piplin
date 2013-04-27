(ns piplin.test.mux
  (:use clojure.test)
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-right bit-shift-left pos? neg? zero?])
  (:use [piplin types math modules mux]
        plumbing.core)
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
  (let [mod (modulize
              :root
              {:flip (fnk [flip]
                          (mux2 flip
                                false
                                true))}
              {:flip false})]
    (is (= (get (last (sim (compile-root mod) 0))
                [:root :flip])
           false))
    (is (= (get (last (sim (compile-root mod) 1))
                [:root :flip])
           true))
    (is (= (get (last (sim (compile-root mod) 2))
                [:root :flip])
           false))
    (is (= (get (last (sim (compile-root mod) 10))
                [:root :flip])
           false))
    (is (= (get (last (sim (compile-root mod) 11))
                [:root :flip])
           true))))

(deftest mux2-connect-test
  (let [mod (modulize
              :root
              {:o (fnk [o]
                       (mux2 o false true))
               :i (fnk [i o]
                       (mux2 o
                             (+ i i)
                             (dec i)))}
              {:o false
               :i ((uintm 8) 3)})]
    (is (= (get (last (sim (compile-root mod) 0))
                [:root :o])
           false))
    (is (= (get (last (sim (compile-root mod) 0))
                [:root :i])
           ((uintm 8) 3)))
    (is (= (get (last (sim (compile-root mod) 1))
                [:root :o])
           true))
    (is (= (get (last (sim (compile-root mod) 1))
                [:root :i])
           ((uintm 8) 2)))
    (is (= (get (last (sim (compile-root mod) 2))
                [:root :o])
           false))
    (is (= (get (last (sim (compile-root mod) 2))
                [:root :i])
           ((uintm 8) 4)))
    (is (= (get (last (sim (compile-root mod) 3))
                [:root :o])
           true))
    (is (= (get (last (sim (compile-root mod) 3))
                [:root :i])
           ((uintm 8) 3)))
    ))

(deftest mux2-connect-test-nested
  (let [e (enum #{:a :b :c})
        mod (modulize
              :root
              {:o (fnk [o]
                       (mux2 o false true))
               :i (fnk [i o]
                       (mux2 o
                             (+ i i)
                             (dec i)))
               :lowbit (fnk [i] (bit-slice (serialize i) 0 1))
               :x (fnk [lowbit o]
                       (mux2 o
                             (mux2 (= lowbit ((bits 1) [1]))
                                   :a
                                   :b)
                             :c))}
              {:i ((uintm 8) 3)
               :x (e :a)
               :o false})]
    (is (= (get (last (sim (compile-root mod) 0))
                [:root :o])
           false))
    (is (= (get (last (sim (compile-root mod) 0))
                [:root :i])
           ((uintm 8) 3)))
    (is (= (get (last (sim (compile-root mod) 0))
                [:root :x])
           (e :a)))
    (is (= (get (last (sim (compile-root mod) 1))
                [:root :o])
           true))
    (is (= (get (last (sim (compile-root mod) 1))
                [:root :i])
           ((uintm 8) 2)))
    (is (= (get (last (sim (compile-root mod) 1))
                [:root :x])
           (e :c)))
    (is (= (get (last (sim (compile-root mod) 2))
                [:root :o])
           false))
    (is (= (get (last (sim (compile-root mod) 2))
                [:root :i])
           ((uintm 8) 4)))
    (is (= (get (last (sim (compile-root mod) 2))
                [:root :x])
           (e :b)))
    (is (= (get (last (sim (compile-root mod) 3))
                [:root :o])
           true))
    (is (= (get (last (sim (compile-root mod) 3))
                [:root :i])
           ((uintm 8) 3)))
    (is (= (get (last (sim (compile-root mod) 3))
                [:root :x])
           (e :c)))))

(deftest cond-connect-test
  (let [e (enum #{:small :medium :big})
        mod (modulize
              :root
              {:i (fnk [i]
                       (inc i))
               :size (fnk [i]
                          (cond
                            (< i 100) (e :small)
                            (< i 200) (e :medium)
                            :else (e :big)))}
              {:size (e :small)
               :i ((uintm 8) 0)})]
    (is (= (get (last (sim (compile-root mod) 10))
                [:root :size])
           (e :small)))
    (is (= (get (last (sim (compile-root mod) 70))
                [:root :size])
           (e :small)))
    (is (= (get (last (sim (compile-root mod) 110))
                [:root :size])
           (e :medium)))
    (is (= (get (last (sim (compile-root mod) 150))
                [:root :size])
           (e :medium)))
    (is (= (get (last (sim (compile-root mod) 220))
                [:root :size])
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
