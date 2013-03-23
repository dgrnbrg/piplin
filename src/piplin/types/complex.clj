(ns piplin.types.complex
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types
         [binops :only [defbinopimpl defcoercions defunopimpl]]
         bits sints])
  (:use [piplin [types :exclude [cast]] protocols])
  (:use [slingshot.slingshot]))

(defpiplintype Complex [real imag])
(defn complex
  "Makes a new complex number type
  with the real part having type `real`
  and the imaginary part having type `imag`.
  `real` and `imag` must support addition,
  subtraction, and multiplication."
  [real imag]
  (merge (Complex. real imag)
         {:kind :complex}))

(defn real-part
  "Returns the real part of a complex number."
  [complex]
  (if (pipinst? complex)
    (first (value complex))
    (mkast (:real (typeof complex))
           :real-part
           [complex]
           real-part)))

(defn imag-part
  "Returns the real part of a complex number."
  [complex]
  (if (pipinst? complex)
    (second (value complex))
    (mkast (:imag (typeof complex))
           :imag-part
           [complex]
           imag-part)))

(defmethod constrain
  :complex
  [complex init-val]
  (let [{:keys [real imag]} complex
        [real-val imag-val] init-val]
    [(instance real (constrain real (value real-val)))
     (instance imag (constrain imag (value imag-val)))]))

(defmethod check
  :complex
  [inst]
  (let [{:keys [real imag]} (typeof inst)
        [real-val imag-val] (value inst)]
    [(check real-val)
     (check imag-val)]))

(defmethod promote
  :complex
  [this obj]
  (let [{:keys [real imag]} this]
    (cond
      (= (typeof obj) this) obj
      (and (vector? obj)
           (= 2 (count obj)))
      (let [[r i] obj]
        (instance this [(promote real r)
                        (promote imag i)]))
      (= (kindof obj) (:kind this))
      (throw+ (error (str "Cannot convert " obj " to " this)))
      :else (throw+ (error "Don't know how to promote to" this "from"
                           (typeof obj))))))

(defbinopimpl impl/+ :complex []
  [x y]
  (let [[r1 i1] (value x)
        [r2 i2] (value y)]
    [(impl/+ r1 r2)
     (impl/+ i1 i2)]))

(defbinopimpl impl/- :complex []
  [x y]
  (let [[r1 i1] (value x)
        [r2 i2] (value y)]
    [(impl/- r1 r2)
     (impl/- i1 i2)]))

(defbinopimpl impl/* :complex []
  [x y]
  (let [[r1 i1] (value x)
        [r2 i2] (value y)]
    [(impl/- (impl/* r1 r2) (impl/* i1 i2))
     (impl/+ (impl/* i1 r2) (impl/* r1 i2))]))

(defmethod bit-width-of
  :complex
  [type]
  (+ (bit-width-of (:real type))
     (bit-width-of (:imag type))))

(defmethod get-bits
  :complex
  [expr]
  (let [[r i] (value expr)]
    (value (bit-cat (serialize r) (serialize i)))))

(defmethod from-bits
  :complex
  [type bits]
  (let [bits (instance
               (piplin.types.bits/bits (count bits))
               bits)
        {:keys [real imag]} type
        imag-len (bit-width-of imag)
        imag-bits (bit-slice bits 0 imag-len)
        real-bits (bit-slice
                    bits
                    imag-len
                    (bit-width-of (typeof bits)))]
    [(from-bits real real-bits) (from-bits imag imag-bits)]))

#_(let [disp #(let [[a b] (value %)
                  a (value a)
                  b (value b)]
              (str (/ a (Math/pow 2 8)) \space
                   (/ b (Math/pow 2 8)) \i))
      sfxpts-type (piplin.types.sfxpts/sfxpts 8 8)
      c1 (piplin.types/cast (complex sfxpts-type sfxpts-type)
                                    [2.0 2.0])
      c2 (piplin.types/cast (complex sfxpts-type sfxpts-type)
                                    [2.8 1.5])]
  (println "c1 =" (disp c1))
  (println "c2 =" (disp c2))
  (println "c1 + c1 =" (disp (impl/+ c1 c1)))
  (println "c1 + c2 =" (disp (impl/+ c1 c2)))
  (println "c2 + c1 =" (disp (impl/+ c1 c2)))
  (println "c1 * c1 =" (disp (impl/* c1 c1)))
  (println "c1 * c2 =" (disp (impl/* c1 c2)))
  (println "c2 * c1 =" (disp (impl/* c1 c2)))
  (println "c1 * c2 * c2 =" (disp (impl/* c1 c2 c2)))
  (println "c1 * c2 * c2  + c1=" (disp (impl/+ c1 (impl/* c1 c2 c2))))
  (deserialize (complex sfxpts-type sfxpts-type) (serialize c1))
  )
