(ns piplin.test.math
  (:use clojure.test
        plumbing.core)
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not and or bit-shift-right bit-shift-left pos? neg? zero?])
  (:use [piplin types math modules protocols])
  (:use [piplin.types bits boolean enum numbers core-impl binops uintm])
  (:import clojure.lang.ExceptionInfo))

(deftest j-long-test
  (is (= 3 (+ 1 2)))
  (is (= 6 (* 2 3)))
  (is (thrown? ExceptionInfo (promote :j-long "lx"))))

(deftest bit-width-of-non-type-test
  (is (thrown? ExceptionInfo (bit-width-of ((uintm 8) 0)))))

(deftest float-test
  (is (= 3.5 (+ 1.5 2))))

(deftest =test
  (is (= 1 1 1))
  (is (not (= 1 1 2)))
  (is (= "hello" "hello"))
  (is (= [1 \a] [1 \a]))
  (is (< 3 7))
  (is (> 0 -1.2))
  (is (>= -3.3 -3.3))
  (is (not (<= 4 1)))
  (is (not= 1 2))
  (is (not (not= 0 0))))

(deftest boolean-test
  (is (not false))
  (is (and))
  (is (not (or)))
  (is (and true true))
  (is (not (and true false)))
  (is (not (and false true)))
  (is (not (and false true true true)))
  (is (not (and true true true false)))
  (is (and true true true true))

  (is ((make-sim-fn (and (uninst true) true))))
  (is ((make-sim-fn (and true (uninst true)))))
  (is ((make-sim-fn (and true true (uninst true)))))
  (is ((make-sim-fn (and (uninst true) true true))))
  (is (make-sim-fn (and (uninst false) true true)))
  (is (not ((make-sim-fn (and (uninst false) true true)))))

  (is (or true false))
  (is (or false true))
  (is (or false true true true))
  (is (or true true true false))
  (is (not (or false false false false)))

  (is ((make-sim-fn (or true (uninst false)))))
  (is ((make-sim-fn (or (uninst true) false))))
  (is ((make-sim-fn (or (uninst true) (uninst false)))))
  (is (make-sim-fn (or (uninst false) false false)))
  (is (make-sim-fn (or false false (uninst false))))
  (is (not ((make-sim-fn (or (uninst false) false false)))))
  (is (not ((make-sim-fn (or false false (uninst false)))))))

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
    (is (= (- 0 (um8 1)) (um8 255)))
    (is ((make-sim-fn (> (uninst (um8 30)) (um8 20)))))
    (is ((make-sim-fn (>= (uninst (um8 20)) (um8 20)))))
    (is (>= (um8 20) (um8 20)))
    (is ((make-sim-fn (<= (uninst (um8 20)) (um8 30)))))
    (is (<= (um8 20) (um8 30)))
    (is (thrown? ExceptionInfo
                 (cast (uintm 8) #b00)))))

(deftest binop-error-test
  (let [e (error "hi")]
    (is (= (try-errors (+ 0 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 2 3 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 (throw+ e) 3 (throw+ e))) e))
    (is (= (try-errors (+ (throw+ e) 0)) e))
    (is (= (try-errors (+ (throw+ e) (throw+ e))) e))
    (is (= (try-errors (bit-cat (throw+ e) (throw+ e))) e))))

(deftest bits-test
  (let [b8 #(promote (bits 8) %)]
    (is (= (value (b8 0xa0)) [1 0 1 0 0 0 0 0]))
    (is (= (value (bit-cat (b8 0xa0) (bit-cat (b8 0xfe))))
           [1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 0]))
    (is (= (value (bit-slice (bit-cat (b8 0xf) (b8 0xf0)) 4 12))
           [1 1 1 1 1 1 1 1]))
    (is (= (value ((make-sim-fn (bit-slice (bit-cat (uninst (b8 0xf)) (b8 0xf0)) 4 12))))
           [1 1 1 1 1 1 1 1]))
    (is (= (bit-and (b8 0xf) 0x3c) (b8 0xc)))
    (is (= (bit-or 0xf0 (b8 0xf)) (b8 0xff)))
    (is (= (bit-xor (instance (uintm 8) 0x3c) 0xf)
           ((uintm 8) 0x33))))
  (is (= (get-bits true) [1]))
  (is (= (get-bits false) [0]))
  (is (= (serialize true) (cast (bits 1) 1)))
  (is (= (serialize false) (cast (bits 1) 0)))
  (is (= (count (value (serialize
                         (promote (enum #{:a :b}) :a))))
         1))
  (is (= (count (value (serialize
                         (promote (enum #{:a :b :c}) :a))))
         2)))

(deftest sim-uintm-bits-test
  (let [mod (modulize
              :root
              {:c (fnk [c] (inc c))
               :d (fnk [c] (bit-slice (serialize c) 0 4))}
              {:c (promote (uintm 8) 0)
               :d (promote (bits 4) 0)})]
    (are [cycle reg val] (= (get (last (sim (compile-root mod) cycle)) [:root reg])
                            val)
         10 :c (instance (uintm 8) 10)
         10 :d (instance (bits 4) [1 0 0 1])
         18 :d (instance (bits 4) [0 0 0 1]))))

(deftest sim-cast-test
  (let [mod (modulize
              :root
              {:new-c (fnk [c] (inc c))
               :c (fnk [new-c] new-c)
               :odd (fnk [new-c]
                         (cast (anontype :boolean)
                               (bit-slice (serialize new-c) 0 1)))}
              {:odd false
               :c (promote (uintm 3) 0)})]
    (are [cycle k v] (= (get (last (sim (compile-root mod) cycle)) [:root k]) v)
         1 :c (promote (uintm 3) 1)
         1 :odd true
         2 :odd false
         3 :odd true
         30 :c (instance (uintm 3) 30 :constrain)
         30 :odd false)))

(deftest sim-equals-test
  (let [mod (modulize
              :root
              {:c (fnk [c] (inc c))
               :triggered (fnk [c]
                               (= (inc c) ((uintm 2) 3)))}
              {:c ((uintm 2) 0)
               :triggered false})]
    (are [cycle value] (= (get (last (sim (compile-root mod) cycle))
                               [:root :triggered])
                          value)
         0 false
         1 false
         2 false
         3 true
         4 false
         5 false
         6 false
         7 true)))

(deftest enum-initialize-test
  (let [e (enum #{:a :b :c})]
    (enum (:keymap e)))
  (is (thrown? ExceptionInfo
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 2) 0)})))
  (is (thrown? ExceptionInfo
               (enum {:a (cast (bits 2) 0)
                      2 (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 3) 1)})))
  (is (thrown? ExceptionInfo
               (enum {:a 0
                      :b (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo
               (enum #{:a 1 "a"}))))
