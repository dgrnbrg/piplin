(ns piplin.test.semantics
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp])
  (:use clojure.test)
  (:use piplin.semantics)
  (:use piplin.modules) 
  (:use [piplin.core :exclude [get-all-registers trace-module make-sim defmodule module]]))

(defmodule broken-counter [n]
  [:feedback [x ((uintm n) 0)]
   :outputs [x true]]
  (connect x (inc x)))

(deftest semantic-test
  (is (= 1 (count (walk-modules
                    (broken-counter 4)
                    piplin.semantics/duplicated-ports?
                    concat)))))
