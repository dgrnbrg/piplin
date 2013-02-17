(ns piplin.types.sfxpts
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types
         [binops :only [defbinopimpl defcoercions defunopimpl]]
         bits sints])
  (:use [piplin [types :exclude [cast]] protocols])
  (:use [slingshot.slingshot]))

(defpiplintype SFxptS [i f])
(defn sfxpts
  "Makes a new sfxpts type object
  with `i` integer bits and `f`
  fractional bits."
  [i f]
  (merge (SFxptS. i f)
         {:kind :sfxpts}))

(defmethod constrain
  :sfxpts
  [sfxpts init-val]
  (let [{:keys [i f]} sfxpts]
    (constrain (sints (+ i f)) init-val)))

(defmethod check
  :sfxpts
  [inst]
  (let [{:keys [i f]} (typeof inst)]
    (check ((sints (+ i f)) (value inst)))))

(defmethod promote
  :sfxpts
  [this obj]
  (let [{:keys [i f]} this]
    (cond
      (= (typeof obj) this) obj
      (= (kindof obj) (:kind this))
      (ast-error this (str "Cannot convert " obj " to " this))
      ;(isa-type? :j-integral (kindof obj))
      ;(instance this (bit-shift-left (promote (anontype :j-long) obj) f))
      (isa-type? :j-double (kindof obj))
      (instance this (long (Math/scalb obj (int f))))
      :else (throw+ (error "Don't know how to promote to :sfxpts from"
                           (typeof obj))))))

(defbinopimpl impl/+ :sfxpts [:j-integral :j-double]
  [x y]
  (+ (value x) (value y)))

(defbinopimpl impl/- :sfxpts [:j-integral :j-double]
  [x y]
  (- (value x) (value y)))

(defbinopimpl impl/* :sfxpts [:j-integral :j-double]
  [x y]
  (let [{:keys [i f]} (typeof x)]
    (bit-shift-right (* (value x) (value y)) f)))

(defmethod bit-width-of
  :sfxpts
  [type]
  (+ (:i type) (:f type)))

(defmethod get-bits
  :sfxpts
  [expr]
  (let [n (bit-width-of (typeof expr))]
    (long-to-bitvec (value expr) n)))

(defmethod from-bits
  :sfxpts
  [type bits]
  (Math/scalb (double (from-bits (sints (bit-width-of type)) bits))
              (int (- (:f type)))))

#_(let [disp #(println (/ (value %) (Math/pow 2 10)))
      x (piplin.types/cast (sfxpts 10 10) 3.75)
      x-squared (impl/* x x)
      x*30 (impl/* x 30)
      x*30d2 (impl/* x 30.2)]
  (println x)
  (println x-squared)
  (disp x-squared)
  (println (Math/pow 3.75 2))
  (println "")
  (disp x*30)
  (println (* 3.75 30))
  (println "")
  (disp x*30d2)
  (println (* 3.75 30.2))

  (println)
  (disp (impl/* 120 x))
  (disp (impl/* 130 x))
  (disp (impl/* 140 x))
  (disp (impl/* 150 x))
  (disp (impl/* 160 x))
  (println)
  (disp (impl/* -120 x))
  (disp (impl/* -130 x))
  (disp (impl/* -140 x))
  (disp (impl/* -150 x))
  (disp (impl/* -160 x))
  (println (serialize (piplin.types/cast (sfxpts 10 10) 3.75)))
  )
