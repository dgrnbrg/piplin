(ns piplin.math
  (:use [slingshot.slingshot])
  (:use [clojure.set :only [map-invert intersection difference]])
  (:require [clojure set [core :as clj]])
  (:refer-clojure :exclude [cast + - * bit-and bit-or bit-xor bit-not inc dec > < >= <= not = not= get assoc assoc-in cond condp])
  (:use (piplin types)))

;Here, we allow nil to participate in the ITyped
;protocol. nil support is incomplete.
(extend-protocol ITyped
  nil
  (typeof [this] (anontype :null))
  (value [this] nil)
  (pipinst? [this] true))

(defn trace
  "Takes a function and an expr and returns an
  expr whose value is the expr, but when simulated
  will run the given function for its side effects
  every time this value is used."
  [f expr]
  (mkast (typeof expr) :noop [expr] #(do (f %) %)))

(defn pr-trace
  "Prints the args followed by the traced value."
  [& args]
  (trace #(println
            (apply print-str (concat (butlast args) [%])))
         (last args)))
