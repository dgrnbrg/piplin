(ns piplin.test.util
  (:use [clojure.java.shell :only [sh]]
        [clojure.java.io :only [file]])
  (:use clojure.test))

(defn icarus-test
  [verilog-test]
  (let [n ".piplin_icarus_test"
        _ (spit (file (str n ".v")) verilog-test)
        iverilog (sh "iverilog"
                     (str "-o" n ".vvp")
                     "-tvvp"
                     (str n ".v"))
        _ (when-not (zero? (:exit iverilog))
            (throw (ex-info "iverilog failed" iverilog)))
        vvp (sh (str "./" n ".vvp"))
        _ (sh "rm" "-f"
              (str n ".vvp")
              (str n ".v"))]
    (is (re-find #"test passed" (:out vvp)) "tests didn't pass")
    (is (= 0 (:exit vvp)) "tested failed-return value")))

