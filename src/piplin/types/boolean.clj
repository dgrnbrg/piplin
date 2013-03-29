(ns piplin.types.boolean
  "This namespaces contains support for booleans. It also
  provides an implementation of `not`."
  (:refer-clojure :exclude [not cast and or = not=])
  (:require [clojure.core :as clj])
  (:use [piplin protocols types])
  (:use [piplin.types bits binops])
  (:use [slingshot.slingshot]))

(derive-type java.lang.Boolean :piplin-type)
(extend-protocol ITyped
  java.lang.Boolean
  (typeof [this] (anontype :boolean))
  (value [this] this)
  (pipinst? [this] true))

(defn not
  "not is important to implement ;-)"
  [x]
  (if (typeof x)
    (if (pipinst? x)
      (clj/not x)
      (mkast (anontype :boolean) :not [x] not))
    (clj/not x)))

(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more] (not (apply = x y more))))

(defn and
  ([] true)
  ([x] x)
  ([x y]
   (if (clj/and (typeof x) (typeof y))
     (if (clj/and (pipinst? x) (pipinst? y))
       (clj/and x y)
       (mkast (anontype :boolean) :and [x y] and))
     (clj/and x y)))
  ([x y & more]
   (if (seq more)
     (recur (and x y) (first more) (next more))
     (and x y))))

(defn or
  ([] false)
  ([x] x)
  ([x y]
   (if (clj/and (typeof x) (typeof y))
     (if (clj/and (pipinst? x) (pipinst? y))
       (clj/or x y)
       (mkast (anontype :boolean) :or [x y] or))
     (clj/or x y)))
  ([x y & more]
   (if (seq more)
     (recur (or x y) (first more) (next more))
     (or x y))))

;(pprint (and true true))
;(def-n-ary-binop or false [:boolean])

(defmethod promote
  :boolean
  [type obj]
  (clj/= 1
     (clj/cond
       (clj/= (typeof obj) type) (if obj 1 0)
       (clj/or (clj/= true obj) (clj/= false obj)) (if obj 1 0)
       (clj/= (typeof obj) (bits 1)) (first (value obj))
       (clj/and (clj/= (kindof obj) :uintm)
            (clj/= (-> obj typeof :n) 1)) (value obj)
       :else
       (throw+ (error "Cannot promote" obj "to boolean")))))

(defmethod bit-width-of
  :boolean
  [type]
  1)

(defmethod get-bits
  :boolean
  [expr]
  (if (value expr) [1] [0]))

(defmethod from-bits
  :boolean
  [type bits]
  (clj/= (first bits) 1))

