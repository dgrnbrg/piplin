(ns piplin.types.uintm
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types
         [binops :only [defbinopimpl defcoercions defunopimpl]]
         bits])
  (:use [piplin [types :exclude [cast]] protocols])
  (:use [slingshot.slingshot]))

;We define the uintm type. This is
;an unsigned integer with modulo on overflow
;or underflow.
(defpiplintype UIntM [n])
(defn uintm
  "Makes a new uintm type object with the
  given number of bits."
  [n]
  (merge (UIntM. n)
         {:kind :uintm}))

;Takes a uintm (this) and the value
;to initialize the new instance with,
;and constrains the number to be in the
;range of uintm
(defmethod constrain
  :uintm
  [this init-val]
  (bit-and init-val (dec (bit-shift-left 1 (:n this)))))

(defmethod check
  :uintm
  [inst]
  (let [n (-> inst typeof :n)
        v (value inst)
        maxval (dec (bit-shift-left 1 n))]
    (when (neg? v)
      (throw+ (error "uintm must be positive:" v)))
    (when (> v maxval)
      (throw+ (error "uintm" n "must be less than" maxval
                     ", got:" v))))
  inst)

(defmethod promote
  :uintm
  [this obj]
  (cond
    (= (typeof obj) this) obj ;Already correct
    (= (kindof obj)
       (:kind this)) 
    (throw+ (error (str "Cannot convert " obj " to " this)))
    (isa-type? :j-integral (kindof obj)) (instance
                                           this
                                           (promote (anontype :j-long)
                                                    obj))
    :else (throw+ (error "Don't know how to promote to :uintm from"
                         (typeof obj)))))

(defbinopimpl impl/+ :uintm [:j-integral]
  [lhs rhs]
  (+ (value lhs) (value rhs)))

(defbinopimpl impl/- :uintm [:j-integral]
  [lhs rhs]
  (- (value lhs) (value rhs)))

(defbinopimpl impl/* :uintm [:j-integral]
  [lhs rhs]
  (* (value lhs) (value rhs)))

(defmethod impl/> [:uintm :uintm]
  [lhs rhs]
  (if (and (pipinst? lhs) (pipinst? rhs))
    (> (value lhs) (value rhs))
    (mkast (anontype :boolean) :> [lhs rhs] impl/>)))
(defcoercions impl/> :uintm [:j-integral])

(defmethod impl/>= [:uintm :uintm]
  [lhs rhs]
  (if (and (pipinst? lhs) (pipinst? rhs))
    (>= (value lhs) (value rhs))
    (mkast (anontype :boolean) :>= [lhs rhs] impl/>=)))
(defcoercions impl/>= :uintm [:j-integral])

(defmethod impl/< [:uintm :uintm]
  [lhs rhs]
  (if (and (pipinst? lhs) (pipinst? rhs))
    (< (value lhs) (value rhs))
    (mkast (anontype :boolean) :< [lhs rhs] impl/<)))
(defcoercions impl/< :uintm [:j-integral])

(defmethod impl/<= [:uintm :uintm]
  [lhs rhs]
  (if (and (pipinst? lhs) (pipinst? rhs))
    (<= (value lhs) (value rhs))
    (mkast (anontype :boolean) :<= [lhs rhs] impl/<=)))
(defcoercions impl/<= :uintm [:j-integral])

(defcoercions piplin.types.binops/= :uintm [:j-integral])


(defbinopimpl impl/bit-and :uintm [:j-integral]
  [lhs rhs]
  (impl/bit-and (value lhs) (value rhs)))

;TODO: (bit-or #b00 #b110010) should throw an elhsception
(defbinopimpl impl/bit-or :uintm [:j-integral]
  [lhs rhs]
  (impl/bit-or (value lhs) (value rhs)))

(defbinopimpl impl/bit-xor :uintm [:j-integral]
  [lhs rhs]
  (impl/bit-xor (value lhs) (value rhs)))

(defbinopimpl impl/bit-shift-left :uintm [:j-integral]
  [lhs rhs]
  (impl/bit-shift-left (value lhs) (value rhs)))

(defbinopimpl impl/bit-shift-right :uintm [:j-integral]
  [lhs rhs]
  (impl/bit-shift-right (value lhs) (value rhs)))

(defunopimpl impl/bit-not :uintm
  [x]
  (bit-not (value x)))

(defmethod bit-width-of
  :uintm
  [type]
  (:n type))

(defmethod get-bits
  :uintm
  [expr]
  (let [n (bit-width-of (typeof expr))]
      (long-to-bitvec (value expr) n)))

(defmethod from-bits
  :uintm
  [type bits]
  (bitvec-to-long bits))
