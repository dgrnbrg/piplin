(ns piplin.test.semantics
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or])
  (:use clojure.test)
  (:use piplin.semantics)
  (:use piplin.modules) 
  (:use [piplin.core :exclude [get-all-registers trace-module make-sim defmodule module]]))

(defn count-errors!
  ([required errors]
   (is (= required (count (collect-ast-errors errors))))) 
  ([errors]
   (count-errors! 1 errors)))

(defmodule broken-counter [n]
  [:feedback [x ((uintm n) 0)]
   :outputs [x true]]
  (connect x (inc x)))

(deftest semantic-test
  (is (= 1 (count (walk-modules
                   (broken-counter 4)
                   piplin.semantics/duplicated-ports?
                   concat)))))

(deftest null-test
  (count-errors! (cast (anontype :null) 3)))

(deftest integer-cast-error
   (count-errors! (cast (uintm 4) ((uintm 5) 2))) 
  (count-errors! (get (cast (array (uintm 4) 4) [0 0 0 0]) ((uintm 4) 2))))

(deftest array-test
  (count-errors! (cast (array (anontype :boolean) 4)
                       (cast
                         (array (anontype :boolean) 2)
                         [false false]))))
