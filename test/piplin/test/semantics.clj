(ns piplin.test.semantics
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-left bit-shift-right])
  (:use clojure.test))

(deftest missing
  (is false "semantics is useless now that I've switched out the modules")
  )
