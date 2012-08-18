(ns piplin.types.null
  (:refer-clojure :exclude [cast])
  (:use [piplin types protocols])
  (:use [piplin.types bits])
  (:use [slingshot.slingshot]))

(extend-protocol ITyped
  nil
  (typeof [this] (anontype :null))
  (value [this] nil)
  (pipinst? [this] true))

(defmethod promote
  :null
  [type obj]
  (when-not (nil? obj)
    (ast-error type (str obj "must be nil"))))

(defmethod from-bits
  :null 
  [type bits]
  nil)

(defmethod bit-width-of
  :null
  [type]
  0)

(defmethod get-bits
  :null
  [expr]
  [])
