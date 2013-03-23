(ns piplin.types.numbers
  (:refer-clojure :exclude [cast])
  (:use [slingshot.slingshot])
  (:use [piplin.types])
  (:use [piplin.protocols]))

(derive-type java.lang.Long :piplin-type)
(derive-type java.lang.Integer :piplin-type)
(derive-type java.lang.Short :piplin-type)
(derive-type java.lang.Byte :piplin-type)
(derive-type java.lang.Float :piplin-type)
(derive-type java.lang.Double :piplin-type)
(derive-type :j-integral :piplin-type)

;Allow the about types to participate in ITyped
(extend-protocol ITyped
  java.lang.Long
  (typeof [this] (anontype :j-long))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Integer
  (typeof [this] (anontype :j-int))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Short
  (typeof [this] (anontype :j-short))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Byte
  (typeof [this] (anontype :j-byte))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Float
  (typeof [this] (anontype :j-float))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Double
  (typeof [this] (anontype :j-double))
  (value [this] this)
  (pipinst? [this] true))

(defmethod promote
  :j-int
  [type obj]
  (condp isa-type? (kindof obj)
    :j-integral (int obj)
    :uintm (let [n (:n (typeof obj))]
             (if (< n 32)
               (value obj)
               (throw+ (error obj "must be 32 bits or less"))))
    (throw+ (error "Cannot promote" obj "to Long"))))

(defmethod promote
  :j-long
  [type obj]
  (condp isa-type? (kindof obj)
    :j-integral (long obj)
    :uintm (value obj)
    (throw+ (error "Cannot promote" obj "to Long"))))

;JVM numeric type promotion rules
(derive-type :j-integral :j-num)

(derive-type Double :j-double)
(derive-type Float :j-float)
(derive-type :j-float :j-num)
(derive-type :j-double :j-num)

(derive-type Byte :j-byte)
(derive-type Short :j-short)
(derive-type Integer :j-int)
(derive-type Long :j-long)

(derive-type :j-byte :j-integral)
(derive-type :j-int :j-integral)
(derive-type :j-short :j-integral)
(derive-type :j-long :j-integral)
