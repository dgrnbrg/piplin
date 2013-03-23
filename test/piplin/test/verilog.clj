(ns piplin.test.verilog
  (:use clojure.test)
  (:use [clojure.pprint :only [pprint]])
  (:use [slingshot.slingshot :only [throw+]])
  (:use piplin.test.util
        plumbing.core)
  (:refer-clojure :as clj :exclude [not= bit-or cond bit-xor + - * bit-and assoc assoc-in inc dec bit-not condp < > <= >= = cast get not and or bit-shift-left bit-shift-right])
  (:use [piplin.types bits boolean bundle enum numbers union core-impl binops uintm])
  (:use [piplin connect types math modules mux sim verilog [seven-segment-decoder :only [seven-seg-tester]]]))

(defn counter [n]
  (modulize
    {:x* (fnk [x*] (inc x*))}
    {:x* ((uintm n) 0)}))

(deftest counter-test
  (icarus-test (verify
                 (counter 8) 100)))

(defn multicounter [x y z]
  (modulize
    {:x-out (fnk [] (:x* ((counter x))))
     :y-out (fnk [] (:x* ((counter y))))
     :z-out (fnk [] (:x* ((counter z))))}
    nil))

(deftest multicounter-test
  (icarus-test (verify
                 (multicounter 1 2 3) 100)))

(defn fib-counter [x]
  (modulize
    {:sub (fnk []
               (:x* ((counter x))))
     :prev (fnk [sub] sub)
     :n (fnk [prev sub]
             (+ prev sub))}
    {:prev ((uintm x) 0)
     :n ((uintm x) 0)}))

(deftest fib-counter-test
  (icarus-test (verify
                 (fib-counter 32) 100)
                 ))

(def delayer
  (modulize {:out (fnk [in] in)} {:out ((uintm 8) 0)}))

(def delayer-holder
  (modulize :root {:out (fnk []
                             (let [counter ((counter 8))
                                   delayer (delayer :in (:x* counter))]
                               (:out delayer)))}
            nil))

(deftest delayer-test
  (icarus-test (verify
                 delayer-holder 50)))

(deftest seven-seg-test
  (icarus-test (verify
                 (seven-seg-tester 1) 10)) 
  (icarus-test (verify
                 (seven-seg-tester 2) 10)) 
  (icarus-test (verify
                 (seven-seg-tester 3) 10)) 
  (icarus-test (verify
                 (seven-seg-tester 4) 30)) 
  (icarus-test (verify
                 (seven-seg-tester 8) 300)) 
  (icarus-test (verify
                 (seven-seg-tester 9) 600))
  (icarus-test (verify
                 (seven-seg-tester 10) 1025)))

(def and-or
  (modulize
    {:c (fnk [c] (inc c))
     :c-processed (fnk [c]
                       (map #(deserialize
                               (anontype :boolean)
                               (bit-slice (serialize c)
                                          % (inc %)))
                            (range 3)))
     :x (fnk [c-processed]
             (let [[a b c] c-processed]
               (and a b)))
     :y (fnk [c-processed]
             (let [[a b c] c-processed]
               (or b c)))}
    {:c ((uintm 3) 0)
     :x false
     :y false}))


(deftest and-or-test
  (icarus-test (verify and-or 20)))
