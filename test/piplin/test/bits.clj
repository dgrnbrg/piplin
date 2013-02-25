(ns piplin.test.sfxpts
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right])
  (:use [piplin.types bundle bits boolean core-impl binops uintm])
  (:use [piplin types mux modules sim connect protocols [verilog :only [modules->verilog+testbench]]])
  (:import clojure.lang.ExceptionInfo) 
  (:use clojure.test
        piplin.test.util))

(deftest bit-shift-test
  (is (= (bit-shift-left #b0011 #b10) #b1100))
  (is (= (bit-shift-right #b11000 #b11) #b00011)))

(defmodule bit-shifter-module
  []
  [:feedback [fixed #b10000
              variable #b00000000000000000000001
              counter ((uintm 3) 0)]]
  (connect counter (inc counter))
  (connect fixed (bit-shift-right fixed 1))
  (connect variable (bit-shift-right
                      variable
                      (serialize counter))))

(deftest bit-shift-synthesis
  (icarus-test (modules->verilog+testbench
                 (bit-shifter-module) 20)))
