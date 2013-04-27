(ns piplin.test.bits
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:use [piplin.types bundle bits boolean core-impl binops uintm])
  (:use [plumbing.core :only [fnk]])
  (:use [piplin types mux modules protocols verilog])
  (:import clojure.lang.ExceptionInfo)
  (:use clojure.test
        piplin.test.util))

(deftest bit-shift-test
  (is (= (bit-shift-left #b0011 #b10) #b1100))
  (is (= (bit-shift-right #b11000 #b11) #b00011)))

(def bit-shifter-module
  (modulize :root
            {:counter (fnk [counter] (inc counter))
             :fixed (fnk [fixed] (bit-shift-right fixed 1))
             :variable (fnk [variable counter]
                            (bit-shift-right variable (serialize counter)))}
            {:counter ((uintm 3) 0)
             :variable #b00000000000000000000001
             :fixed #b10000}))

(deftest bit-shift-synthesis
  (icarus-test (verify bit-shifter-module 20)))
