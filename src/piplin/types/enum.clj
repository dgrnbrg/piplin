(ns piplin.types.enum
  "This namespace provides enum support. Enums are
  a way to represent a constrained set of keywords.
  They also allow you to define the bit patterns for
  each keyword.
  
  This namespace also adds keywords to the piplin type
  system so that they can be coerced to enum elements."
  (:refer-clojure :exclude [= not= cast])
  (:use [slingshot.slingshot])
  (:use [piplin protocols types])
  (:use [piplin.types binops bits]))

(derive-type clojure.lang.Keyword :piplin-type)

(extend-protocol ITyped 
  clojure.lang.Keyword 
  (typeof [this] (anontype :keyword)) 
  (value [this] this) 
  (pipinst? [this] true))

(defcoercions = :enum [:keyword])

(defpiplintype PiplinEnum [keymap])
(defn enum
  "Takes a collection of keywords or a map of
  keywords to bits and returns it as an enum
  type."
  [coll & more]
  (let [allow-dups (some #{:allow-dups} more)]
    (if (set? coll)
      (if (every? keyword? coll)
        (let [n (count coll)
              logn (log2 n)]
          (merge (PiplinEnum.
                   (zipmap coll
                           (map #((bits logn) (long-to-bitvec % logn))
                                (iterate inc 0))))
                 {:kind :enum}))
        (throw+ (error
                  "Set must be made of only kewords"
                  (remove keyword? coll))))
      (if (map? coll)
        (cond
          (some #(not= (kindof %) :bits) (vals coll))
          (throw+ (error "Maps values must all be bits")) 
          (some #(not= (-> (seq coll)
                             first
                             val
                             typeof
                             bit-width-of)
                           (bit-width-of (typeof %)))
                (vals coll))
          (throw+ (error
                    "Map's values must be same bit width")) 
          (some #(not (keyword? %)) (keys coll))
          (throw+ (error "Map's keys must be keywords")) 
          (and
            (not allow-dups)
            (not= (count (vals coll))
                      (count (distinct
                               (vals coll)))))
          (throw+ (error "Enum keys must be distinct"))
          :else
          (merge (PiplinEnum. coll)
                 {:kind :enum}))))))

(defmethod promote
  :enum
  [type obj]
  (cond
    (= (typeof obj) type) obj
    (and (keyword? obj)
         (obj (:keymap type))) (instance type obj)
    :else
    (throw+ (error "Cannot promote" obj "to" type))))

(defmethod from-bits
  :enum
  [type bits]
  (let [lookup (reduce (fn [m [k v]]
                         (assoc m (value v) k))
                       {}
                       (:keymap type))
        k (get lookup bits)]
    (cond
      (nil? k)
      (throw+ (error bits "is not a valid element of the enum" (typeof bits)))
      :else
      k)))

(defmethod bit-width-of
  :enum
  [type]
  (-> type 
    :keymap
    vals
    first
    typeof
    bit-width-of))

(defmethod get-bits
  :enum
  [expr]
  (let [type (-> (typeof expr)
               :keymap
               seq
               first
               val
               typeof)]
    (value ((value expr) (:keymap (typeof expr))))))

(defmethod check
  :enum
  [inst]
  (let [keymap (-> inst typeof :keymap)
        v (value inst)]
    (when-not (some #{v} (keys keymap))
      (throw+ (error v "is not in" (keys keymap)))))
  inst)
