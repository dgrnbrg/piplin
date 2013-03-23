(ns piplin.core
  (:refer-clojure :exclude [cast + - * <= < >= > not= = bit-and bit-or bit-xor bit-not inc dec not cond condp and or bit-shift-left bit-shift-right])
  (:use [slingshot.slingshot])
  (:use [swiss-arrows.core :only [-<>]]))

(defn redefine*
  [sym]
  (let [ns-sym (symbol (namespace sym))
        sym (symbol (name sym))]
    (require ns-sym)
    (if-let [var ((ns-publics ns-sym) sym)]
      (let [sym (-<> (meta var)
                     (assoc <> :ns *ns*)
                     (with-meta sym <>))]
        (if (.hasRoot ^clojure.lang.Var var)
          (intern *ns* sym @var)
          (intern *ns* sym)))
      (throw+ (str ns-sym \/ sym " does not exist")))))

(defmacro redefine
  [& symbols]
  (cons `do (map (fn [sym] `(redefine* '~sym)) symbols)))


(require 'piplin.connect)
(defn connect
  [& args]
  (apply piplin.connect/connect args))
(alter-var-root
  #'connect
  #(with-meta % (assoc (meta #'piplin.connect/connect)
                       :ns *ns*)))

;Other important inclusions
(require 'piplin.types.null)

;This is the list of functions we should reexport
(redefine
  ;Module stuff
  piplin.modules/defmodule
  piplin.modules/module
  piplin.modules/make-sim
  piplin.modules/get-all-registers
  piplin.modules/trace-module
  piplin.modules/input
  piplin.modules/modulize
  piplin.modules/compile-root

  ;Sim stuff -- this might be too low level
  piplin.sim/exec-sim
  piplin.sim/trace-keys
  piplin.modules/sim

  ;functions
  piplin.mux/mux2
  piplin.mux/cond
  piplin.mux/condp
  piplin.types/kindof
  piplin.types/anontype
  piplin.types/log2
  piplin.types/cast
  piplin.types.binops/=
  piplin.types.binops/not=
  piplin.types.bits/bits
  piplin.types.bits/bit-width-of
  piplin.types.bits/serialize
  piplin.types.bits/deserialize
  piplin.types.core-impl/bit-and
  piplin.types.core-impl/bit-or
  piplin.types.core-impl/bit-xor
  piplin.types.core-impl/bit-not
  piplin.types.core-impl/bit-shift-left
  piplin.types.core-impl/bit-shift-right
  piplin.types.bits/bit-cat
  piplin.types.bits/bit-slice
  piplin.types.boolean/not
  piplin.types.boolean/and
  piplin.types.boolean/or
  piplin.types.bundle/bundle
  piplin.types.core-impl/+
  piplin.types.core-impl/-
  piplin.types.core-impl/*
  piplin.types.core-impl/inc
  piplin.types.core-impl/dec
  piplin.types.core-impl/>
  piplin.types.core-impl/<
  piplin.types.core-impl/>=
  piplin.types.core-impl/<=
  piplin.types.enum/enum
  piplin.types.uintm/uintm
  piplin.types.sints/sints
  piplin.types.sints/sign-extend
  piplin.types.sfxpts/sfxpts
  piplin.types.complex/complex
  piplin.types.complex/real-part
  piplin.types.complex/imag-part
  piplin.types.union/union
  piplin.types.union/union-match
  piplin.types.union/maybe
  piplin.types.array/array

  ;vcd
  piplin.vcd/spit-trace
  piplin.vcd/trace->gtkwave

  ;verilog
  piplin.verilog/->verilog
  piplin.verilog/verify)

