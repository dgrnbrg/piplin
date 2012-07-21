(ns piplin.types.boolean
  (:refer-clojure :exclude [not cast])
  (:require [clojure.core :as clj])
  (:use [piplin.types])
  (:use [piplin.types.bits])
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

(defmethod promote
  :boolean
  [type obj]
  (clj/= 1
     (clj/cond
       (clj/= (typeof obj) type) (if obj 1 0)
       (or (clj/= true obj) (clj/= false obj)) (if obj 1 0)
       (clj/= (typeof obj) (bits 1)) (first (value obj))
       (and (clj/= (kindof obj) :uintm)
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
  (= (first bits) 1))

