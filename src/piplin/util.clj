(ns piplin.util
  (:require [clojure.set]))

(defn sym-diff
  "symmetric different of 2 sets"
  [s1 s2]
  (clojure.set/union
    (clojure.set/difference s1 s2)
    (clojure.set/difference s2 s1)))
