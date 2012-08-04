(ns piplin.test.core
  (:require [piplin.core :as p])
  (:use [clojure.test]))

(p/defmodule counter [b]
  [:outputs [x ((p/uintm b) 0)]]
  (p/connect x (p/inc x)))

(deftest counter-test
  (let [[state fns] (p/make-sim (counter 8))
        result (p/exec-sim state fns 10)]
    (is (= (get result [:x]) ((p/uintm 8) 10)))))
