(ns piplin.test.verilog
  (:use clojure.test)
  (:use [clojure.pprint :only [pprint]])
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :as clj :exclude [not= bit-or cond bit-xor + - * bit-and assoc assoc-in inc dec bit-not condp < > <= >= = cast get not])
  (:use [clojure.java.shell :only [sh]])
  (:use [piplin types math modules sim verilog]))

(defn module->verilog+testbench
  [mod cycles & keys]
  (let [[state fns] (make-sim mod)
        [fns trace] (apply trace-keys fns keys)
        _ (exec-sim state fns cycles)
        ]
    (str (module->verilog mod)
         (make-testbench mod @trace))))

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
    (is (= 0 (:exit iverilog)) "compilation failed")
    (is (re-find #"test passed" (:out vvp)) "tests didn't pass")
    (is (= 0 (:exit vvp)) "tested failed-return value")))

(defmodule counter [n]
  [:outputs [x ((uintm n) 0)]]
  (connect x (inc x)))

(deftest counter-test
  (icarus-test (module->verilog+testbench
                 (counter 8) 10 [:x]))) 
