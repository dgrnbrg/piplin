(ns piplin.types.sints
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types
         [binops :only [defbinopimpl defcoercions defunopimpl]]
         bits])
  (:use [piplin [types :exclude [cast]] protocols])
  (:use [slingshot.slingshot]))

(defpiplintype SIntS [n])
(defn sints
  "Makes a new sints type object with the
  given number of bits."
  [n]
  (merge (SIntS. n)
         {:kind :sints}))

(defn- bounds
  "Returns the min and max value for a signed
  number of `n` bits."
  [n]
  (let [base (bit-shift-left 1 (dec n))]
    {:min (- base)
     :max (dec base)}))

(defn min-value
  "Returns the minimum value possible to store in `type`"
  [type]
  (let [{:keys [min]} (bounds (:n type))]
    (type min)))

(defn max-value
  "Returns the maximum value possible to store in `type`"
  [type]
  (let [{:keys [max]} (bounds (:n type))]
    (type max)))

(defmethod constrain
  :sints
  [sintm init-val]
  (let [{:keys [min max]} (bounds (:n sintm))]
    (cond
      (< init-val min) min
      (> init-val max) max
      :else init-val)))

(defmethod check
  :sints
  [inst]
  (let [n (-> inst typeof :n)
        v (value inst)
        {:keys [min max]} (bounds n)]
    (when (> v max)
      (throw+ (error "sints" n "is too big:" v)))
    (when (< v min) 
      (throw+ (error "sints" n "is too small:" v)))
    inst))

(defmethod promote
  :sints
  [this obj]
  (cond
    (= (typeof obj) this) obj
    (= (kindof obj) (:kind this))
    (ast-error this (str "Cannot convert " obj " to " this))
    (isa-type? :j-integral (kindof obj))
    (instance this (promote (anontype :j-long) obj))
    :else (throw+ (error "Don't know how to promote to :sints from"
                         (typeof obj)))))


(defbinopimpl impl/+ :sints [:j-integral]
  [x y]
  (+ (value x) (value y)))

(defbinopimpl impl/- :sints [:j-integral]
  [x y]
  (- (value x) (value y)))

(defbinopimpl impl/* :sints [:j-integral]
  [x y]
  (* (value x) (value y)))

(defmethod impl/> [:sints :sints]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (> (value x) (value y))
    (mkast (anontype :boolean) :> [x y] impl/>)))
(defcoercions impl/> :sints [:j-integral])

(defmethod impl/>= [:sints :sints]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (>= (value x) (value y))
    (mkast (anontype :boolean) :>= [x y] impl/>=)))
(defcoercions impl/>= :sints [:j-integral])

(defmethod impl/< [:sints :sints]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (< (value x) (value y))
    (mkast (anontype :boolean) :< [x y] impl/<)))
(defcoercions impl/< :sints [:j-integral])

(defmethod impl/<= [:sints :sints]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (<= (value x) (value y))
    (mkast (anontype :boolean) :<= [x y] impl/<=)))
(defcoercions impl/<= :sints [:j-integral])

(defcoercions piplin.types.binops/= :sints [:j-integral])

(defmethod bit-width-of
  :sints 
  [type]
  (:n type))

(defmethod get-bits
  :sints 
  [expr]
  (let [n (bit-width-of (typeof expr))]
      (long-to-bitvec (value expr) n)))

(defmethod from-bits
  :sints
  [type bits]
  (let [n (:n type)
        [sign & body] (value bits)
        body (bitvec-to-long body)
        {:keys [min]} (bounds n)]
    (if (= 0 sign)
      body
      (+ min body))))
