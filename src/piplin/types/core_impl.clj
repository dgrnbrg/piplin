(ns piplin.types.core-impl
  "This namespace provides multimethod implementations of many
  clojure core functions. It has them by default dispatch to clojure's
  core implementation, but opens the possibility to adding support for
  many bitwise and numeric types in piplin.

  There are also several macros useful for making unary and binary functions
  that delegate to clojure.core implementations, which is useful for
  adding support for currently unsupported operations."
  (:refer-clojure :exclude [= not= bit-and bit-or bit-xor bit-not + - * inc dec > >= < <= cast and or bit-shift-left bit-shift-right])
  (:require [piplin.types])
  (:require [piplin.types.numbers])
  (:require [clojure.core :as clj])
  (:use [slingshot.slingshot])
  (:use [piplin.types.binops]))

(defn- make-core-unop-fn
  "Makes syntax for a unop using the core
  implementation for a given hierarchy key"
  [op key]
  `(defmethod ~op ~key
     [~'x]
     (~(symbol "clojure.core" (name op)) ~'x)))

(defn- make-core-binop-fn
  "Makes syntax for a binop using the core
  implementation for a given hierarchy key"
  [op key]
  `(defmethod ~op [~key ~key]
     [~'x ~'y]
     (~(symbol "clojure.core" (name op)) ~'x ~'y)))

(defmacro def-n-ary-binop
  "Defines a generic function for a binary operation
  on numeric types. Uses the function in clojure/core
  to provide implementations for Numbers. Handles
  nullary and n-ary invocations by returning zero and
  the left-associative folds, respectively. Existing-types
  is a vector of hierarchy elements whose implementations
  in clojure.core should be integrated."
  [op zero existing-types]
  (let [core-methods (map #(make-core-binop-fn
                             op %)
                          existing-types)
        core-unary (map #(make-core-unop-fn
                           op %)
                        existing-types)]
    `(do
       (defmulti ~op nary-dispatch :hierarchy piplin.types/types)
       (defmethod ~op :piplin.types.binops/nullary [] ~zero)
       (defmethod ~op :piplin.types.binops/n-ary
         [~'x ~'y & ~'more]
         (if (seq ~'more)
           (recur (~op ~'x ~'y) (first ~'more) (next ~'more))
           (~op ~'x ~'y)))
       ~@core-methods
       ~@core-unary)))

(defmacro def-binary-binop
  "Like def-n-ary-binop, but without nullary or left-associative
  folds."
  [op existing-types]
  (let [core-methods (map #(make-core-binop-fn
                             op %)
                          existing-types)]
    `(do
       (defmulti ~op binary-dispatch :hierarchy piplin.types/types)
       ~@core-methods)))

(def-n-ary-binop + 0 [:j-num])
(def-n-ary-binop - 0 [:j-num])
(def-n-ary-binop * 0 [:j-num])
(def-n-ary-binop bit-and 0 [:j-num])
(def-n-ary-binop bit-or 0 [:j-num])
(def-n-ary-binop bit-xor 0 [:j-num])

(defmulti bit-not
  piplin.types/piplin-clojure-dispatch
  :hierarchy piplin.types/types)
(defmethod bit-not :use-core-impl
  [x]
  (clj/not x))
(defmethod bit-not :default
  [x]
  (clj/not x))

(defn inc
  "Increments x"
  [x]
  (+ x 1))

(defn dec
  "Decrements x"
  [x]
  (- x 1))

;Define the given math operators as extensible piplin operators
;that support AST generation
(def-binary-binop > [:j-num])
(def-binary-binop < [:j-num])
(def-binary-binop >= [:j-num])
(def-binary-binop <= [:j-num])
(def-binary-binop bit-shift-left [:j-num])
(def-binary-binop bit-shift-right [:j-num])
