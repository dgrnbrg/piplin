(ns piplin.test.verilog
  (:use clojure.test)
  (:use [clojure.pprint :only [pprint]])
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :as clj :exclude [not= bit-or cond bit-xor + - * bit-and assoc assoc-in inc dec bit-not condp < > <= >= = cast get not])
  (:use [clojure.java.shell :only [sh]])
  (:use [piplin.types bits boolean bundle enum numbers union core-impl binops uintm])
  (:use [piplin connect types math modules mux sim verilog [seven-segment-decoder :only [seven-seg-tester]]]))

(defn icarus-test
  [verilog-test]
  (let [n ".piplin_icarus_test"
        _ (spit (java.io.File. (str n ".v")) verilog-test)
        iverilog (sh "iverilog"
                     (str "-o" n ".vvp")
                     "-tvvp"
                     (str n ".v"))
        vvp (sh (str "./" n ".vvp"))
        _ (sh "rm" "-f"
              (str n ".vvp")
              (str n ".v"))]
    (is (= 0 (:exit iverilog)) iverilog)
    (is (re-find #"test passed" (:out vvp)) "tests didn't pass")
    (is (= 0 (:exit vvp)) "tested failed-return value")))

(defmodule counter [n]
  [:outputs [x ((uintm n) 0)]]
  (connect x (inc x)))

(deftest counter-test
  (icarus-test (modules->verilog+testbench
                 (counter 8) 100)))

(defmodule multicounter [x y z]
  [:modules [foo (counter x)
             bar (counter y)
             baz (counter z)]])

(deftest multicounter-test
  (icarus-test (modules->verilog+testbench
                 (multicounter 1 2 3) 100)))

(defmodule fib-counter [x]
  [:modules [c (counter x)]
   :feedback [prev ((uintm x) 0)]
   :outputs [n ((uintm x) 0)]]
  (connect prev (subport c :c :x))
  (connect n (+ prev (subport c :c :x))))

(deftest fib-counter-test
  (icarus-test (modules->verilog+testbench
                 (fib-counter 32) 100)))

(defmodule delayer []
  [:inputs [in (uintm 8)]
   :outputs [out ((uintm 8) 0)]]
  (connect out in))

(defmodule delayer-holder []
  [:modules [c (counter 8)
             d (delayer)]]
  (connect (subport d :d :in) (subport c :c :x)))

(deftest delayer-test
  (icarus-test (modules->verilog+testbench
                 (delayer-holder) 50)))

(deftest seven-seg-test
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 1) 10)) 
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 2) 10)) 
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 3) 10)) 
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 4) 30)) 
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 8) 300)) 
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 9) 600))
  (icarus-test (modules->verilog+testbench
                 (seven-seg-tester 10) 1025)))
