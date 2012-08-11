(ns piplin.types.binops
  "This namespace contains functions for doing binary and n-ary
  dispatch, and macros for generating automatic type promotion
  for binops. It also defines `=` and `not=` as multimethods."
  (:refer-clojure :exclude [= not= cast])
  (:require [clojure.core :as clj])
  (:use [slingshot.slingshot])
  (:use [piplin types]))

(defn binary-dispatch
  "Simplified binary dispatch logic."
  [x y]
  [(piplin-clojure-dispatch x)
   (piplin-clojure-dispatch y)])

(defn nary-dispatch
  "Dispatching logic used by binary math operations"
  ([] ::nullary)
  ([x] (piplin-clojure-dispatch x))
  ([x y] (binary-dispatch x y))
  ([x y & more] ::n-ary))

(defn- make-binop-impl-fn
  "Takes a kind, fntail, and an symbol for the op
  and returns a function which should be invoked
  with 2 arguments of the appropriate kind, and
  it will evaluate them if the arguments are
  immediate and it will return an AST expr if one
  or more of the arguments aren't immediates."
  [op unmangled-kw k fntail]
  `(defmethod ~op [~k ~k]
     [x# y#]
     (if (and (pipinst? x#)
              (pipinst? y#))
       (instance (typeof x#)
                 ((fn ~@fntail) x# y#)
                 :constrain)
       (let [~'lhs x# ~'rhs y#]
         (->
           (mkast (typeof x#)
                ~unmangled-kw
                [~'lhs ~'rhs] ~op)
           (assoc-dist-fn
             #(~op (cast % ~'lhs) (cast % ~'rhs))))))))

(defmacro defunopimpl
  [op k & [args & fntail]]
  (assert (clj/= 1 (count args)))
  (let [arg-name (first args)
        unmangled-kw (keyword (name op))]
    `(defmethod ~op ~k
       ~args
       (if (pipinst? ~arg-name)
         (instance (typeof ~arg-name)
                   ((fn ~args ~@fntail) ~arg-name)
                   :constrain)
         (mkast (typeof ~arg-name) ~unmangled-kw ~args ~op)))))

(defn make-binop-explict-coercions
  "Takes a kind and a vector of kinds and returns
  a list of syntax for defmethods that will invoke
  the method for [kind kind] if kind is one of the
  arguments and any element of the vector is the
  other argument's kind."
  [op k bases]
  (let [k-bases (map #(vector k %) bases)
        dispatches (concat k-bases
                           (map reverse k-bases))]
    (map (fn [[a b]]
           `(defmethod ~op [~a ~b]
              [~'x ~'y]
              (let [[~'x  ~'y] (type-unify ~k ~'x ~'y)]
                (~op ~'x ~'y))))
         dispatches)))

(defmacro defcoercions
  "Used to construct automatic coercion for binop types
  when they don't follow the more common return value
  pattern."
  [op k bases]
  `(do ~@(make-binop-explict-coercions op k bases)))

(defmacro defbinopimpl
  "Defines implementation of a binop on a piplin kind.
  Requires the kind to attempt to unify to, the list
  of types which can be promoted to the kind, and an
  fntail that takes 2 arguments and returns the result.
  The implementation will return an error if the
  unification failed."
  [op k bases & fntail]
  (let [unmangled-kw (keyword (name op))
        impl-body (make-binop-impl-fn op unmangled-kw
                                      k fntail)
        bodies (make-binop-explict-coercions op k bases)]
    `(do
       ~impl-body
       ~@bodies)))

(defmulti =
  "= is a very common function. It must be
  implemented explicitly (rather than using
  the def-binary-binop function) in order to
  explicitly check whether they're both not
  ASTNodes, and if so to delegate to Clojure's
  =. This is because not every object subject to
  = participates in the piplin typesystem,
  whereas all numbers do participate, so this
  isn't an issue for >, <, <=, >=, etc."
  nary-dispatch :hierarchy types)
(defmethod = :use-core-impl [x]
  (clj/= x))
(defmethod = [:use-core-impl :use-core-impl] [x y]
  (clj/= x y))
(defmethod = :default [x y]
  (clj/cond
    (and (nil? x) (nil? y))
    true
    (or (nil? x) (nil? y))
    false
    (not (or (instance? piplin.types.ASTNode x)
             (instance? piplin.types.ASTNode y)))  
    (clj/= x y)
    (and (pipinst? x) (pipinst? y))
    (let [[x y] (type-unify (kindof x) x y)]
      (clj/= (value x) (value y)))
    :else
    (mkast (anontype :boolean) := [x y] =)))
(defmethod = ::n-ary
  [x y & more]
  (if (= x y)
    (if (seq more)
      (recur x (first more) (rest more))
      true)
    false))

(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more] (not (apply = x y more))))
