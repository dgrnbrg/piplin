(ns piplin.test.johnson
  (:use piplin.test.util)
  (:use clojure.test)
  (:require [piplin.core :as p]))

(p/defmodule johnson [w]
  [:outputs [q (p/cast (p/bits w) 0)]]
  (let [q' (p/bit-cat (p/bit-slice q 0 (dec w))
                      (p/bit-not (p/bit-slice q (dec w) w)))]
    (p/connect q  q')))

(deftest basic-johnson-test
  (let [m (johnson 4)
        [state fns] (p/make-sim m)]
    (is (= (get (p/exec-sim state fns 0) [:q])
           #b0000))
    (is (= (get (p/exec-sim state fns 1) [:q])
           #b0001))
    (is (= (get (p/exec-sim state fns 2) [:q])
           #b0011))
    (is (= (get (p/exec-sim state fns 3) [:q])
           #b0111))
    (is (= (get (p/exec-sim state fns 4) [:q])
           #b1111))
    (is (= (get (p/exec-sim state fns 5) [:q])
           #b1110))
    (is (= (get (p/exec-sim state fns 6) [:q])
           #b1100))
    (is (= (get (p/exec-sim state fns 7) [:q])
           #b1000))
    (is (= (get (p/exec-sim state fns 8) [:q])
           #b0000))
    ))

(deftest basic-johnson-verilog-test
  (icarus-test (p/modules->verilog+testbench
                 (johnson 8) 500)))
