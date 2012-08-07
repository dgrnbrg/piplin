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
      (instance type (into {}
                           (map (fn [[k v]]
                                  [k (promote (:array-type type) v)])
                                s))))
    (do
      (when-not (seqable? obj)
        (throw+ "Can only promote seqables to piplin array"))
      (let [s (seq obj)
            {array-type :array-type array-len :array-len} type]
        (when-not (= (count s) array-len)
          (throw+ (str "Expected " array-len
                       " elements, but got "
                       (count s) "elements")))
        (let [casted-obj (zipmap (->> (range array-len)
                                   (map (comp keyword str)))
                                 (map (partial cast array-type)
                                      s))] 
          (if (every? pipinst? (vals casted-obj))
            (instance type casted-obj :constrain)
            (mkast-explicit-keys type :make-array
                                 (map keyword (range array-len))
                                 casted-obj
                                 (fn [& args]
                                   (promote type args)))))))))

(defmethod check
  :array
  [obj]
  (when (and (pipinst? obj)
             (some (comp not pipinst?)  (vals (value obj))))
    (throw+ (error "Array looks like a pipinst, but contains non-pipinst values:" (vals (value obj)))))
  obj)

(defmethod bit-width-of
  :array
  [{:keys [array-type array-len]}]
  (* array-len (bit-width-of array-type)))

(defmethod get-bits
  :array
  [expr]
  (let [inst (value expr)
        keys (->> (typeof expr)
               :array-len
               range
               (map (comp keyword str)))]
    (->> keys
      (map #(serialize (inst %1)))
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
     (get (value array) (-> i str keyword) notfound)
     (get array i))))

;TODO: ensure that i is the correct bit width, or a constant
(defmethod piplin.types/valAt-multi
  :array
  ([array i]
   (piplin.types/valAt-multi array i nil))
  ([array i notfound]
   (if (and (pipinst? i)
            (pipinst? array))
     (get (value array) (-> i value str keyword) notfound)
     (mkast (:array-type (typeof array))
            :array-get 
            [array i]
            get))))

(defmethod piplin.types/entryAt-multi
  :array
  ([array i]
   (let [alen (:array-len (typeof array))]
     (when (>= i alen)
       (throw+ (str "Array is only " alen
                    "long; tried to access index " i))) 
     (nth array i))))

(defmethod piplin.types/assoc-multi
  :array
  [array index v]
  (let [{:keys [array-type array-len]} (typeof array)
        v (cast array-type v)]
    (when (or (neg? index)
              (>= index array-len)))
    (if (and (pipinst? array)
             (pipinst? index)
             (pipinst? v))
      ((typeof array) (assoc (value array) index v))
      (mkast (typeof array)
             :array-assoc
             [array index v]
             assoc))))

(defmethod piplin.types/count-multi
  :array
  [array]
  (:array-len (typeof array)))
