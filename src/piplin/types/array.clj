(ns piplin.types.array
  "This namespace provides a fixed-length homogenous array type."
  (:refer-clojure :exclude [cast])
  (:use [slingshot.slingshot])
  (:use [piplin.types])
  (:use [clojure.core.incubator :only [seqable?]])
  (:use [piplin.types.bits]))

(defpiplintype Array [array-len array-type])
(defn array
  [type length]
  (when-not (integer? length)
    (throw+ "Length must be an integer"))
  (when-not (:kind type)
    (throw+ (str type " must be a piplin type")))
  (merge (Array. length type)
         {:kind :array}))

(defmethod promote
  :array
  [type obj]
  (if (and (pipinst? obj)
           (= (kindof obj) :array))
    (let [{atype :array-type alen :array-length} (typeof obj)
          s (value obj)]
      (when-not (= alen (:array-length type))
        (throw+ (error "Array is not of length"
                       (:array-length type)
                       "=>" obj)))
      (instance type (map (partial promote
                                   (:array-type type))
                          s)))
    (do
      (when-not (seqable? obj)
        (throw+ "Can only promote seqables to piplin array"))
      (let [s (seq obj)
            {array-type :array-type array-len :array-len} type]
        (when-not (= (count s) array-len)
          (throw+ (str "Expected " array-len
                       " elements, but got "
                       (count s) "elements")))
        (let [casted-obj (mapv (partial cast array-type) s)] 
          (if (every? pipinst? s)
            (instance type casted-obj :constrain)
            (mkast-explicit-keys type :make-array
                                 (map keyword (range array-len))
                                 casted-obj
                                 (fn [& args]
                                   (promote type args)))))))))

(defmethod bit-width-of
  :array
  [{:keys [array-type array-len]}]
  (* array-len (bit-width-of array-type)))

(defmethod get-bits
  :array
  [expr]
  (let [inst (value expr)]
    (->> inst
      (map serialize)
      (apply bit-cat)
      value)))

(defmethod from-bits
  :array
  [type bs]
  (let [{array-type :array-type array-len :array-len} type
        objs (partition (bit-width-of array-type) bs)]
    (mapv (partial from-bits array-type) objs)))

(defmethod piplin.types/nth-multi
  :array
  ([array i]
   (piplin.types/nth-multi array i nil))
  ([array i notfound]
   (if (pipinst? array)
     (nth (value array) i notfound)
     (mkast (:array-type (typeof array))
            :array-nth
            [array i]
            nth))))

(defmethod piplin.types/assoc-multi
  :array
  [array index v]
  (let [{:keys [array-type array-len]} (typeof array)
        v (cast array-type v)]
    (when (or (neg? index)
              (>= index array-len)))
    (if (pipinst? array)
      ((typeof array) (assoc (value array) index v))
      (mkast (typeof array)
             :array-assoc
             [array index v]
             assoc))))

(defmethod piplin.types/count-multi
  :array
  [array]
  (:array-len (typeof array)))
