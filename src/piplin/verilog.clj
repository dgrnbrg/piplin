(ns piplin.verilog
  (:use [slingshot.slingshot])
  (:use [clojure.walk :only [postwalk]]) 
  (:use fleet) 
  (:use [clojure.set :only [map-invert]])
  (:use [clojure.string :only [join]]) 
  (:use [piplin modules types])
  (:use [piplin [math :only [bit-width-of piplin-clojure-dispatch]]]))


(def uintm-add-template
  (fleet [bits x y out]
         "uintm_add #(<(str bits)>)
           <(gensym \"uintm_add\")>(
             .x(<(str x)>),
             .y(<(str y)>),
             .out(<(str out)>));"))

;example
(str (uintm-add-template 1 2 3 4))

(defn make-union-verilog
  [tag padding value]
  (str "{" tag
       (when (pos? padding)
         (str ", " padding "'b" 0)) 
       ", " value "}"))

(defmulti verilog-repr
  kindof)
(defmethod verilog-repr :default
  [x]
  (throw+ (error "cannot convert to verilog:" x)))

(defmethod verilog-repr :uintm
  [x]
  (let [t (typeof x)
        i (value x)
        w (bit-width-of t)] 
    (str w "'d" i)))

(defmethod verilog-repr :boolean
  [x]
  (if x "1'b1" "1'b0"))

(defmethod verilog-repr :bits
  [x]
  (let [t (typeof x)
        b (value x)
        w (bit-width-of t)]
    (str w "'b" (join b))))

(defmethod verilog-repr :enum
  [x]
  (let [t (typeof x)
        keymap (:keymap t)
        e (value x)
        w (bit-width-of t)]
    (str (verilog-repr (keymap e)))))

(defmethod verilog-repr :union
  [x]
  (let [t (typeof x)
        {:keys [schema enum]} t
        v (first (value x)) 
        tag (key v)
        v (val v)
        w (bit-width-of t)
        padding (- w
                   (bit-width-of enum)
                   (bit-width-of (tag schema)))]
    (make-union-verilog
      (verilog-repr (enum tag))
      padding
      (verilog-repr v)
      )))

(defmethod verilog-repr :bundle
  [x]
  (verilog-repr (piplin.math/serialize x)))

(defn lookup-expr
  [table expr]
  (if (pipinst? expr)
    (verilog-repr expr)
    (if-let [name (get table expr)]
      name
      (do
       (throw+ (error expr "not found in" table))))))

(defmulti verilog-of
  (fn [ast name-lookup] (if (pipinst? ast)
                          ::immediate
                          (:op (value ast)))))

(defmethod verilog-of :default
  [ast name-lookup]
  (throw+ (error "cannot convert to verilog:" ast)))

(defmethod verilog-of ::immediate
  [ast name-lookup]
  (verilog-repr ast))

(defmethod verilog-of :port
  [ast name-lookup]
  (lookup-expr name-lookup ast))

(defn merged-args
  [ast]
  (apply merge (map (value ast) [:args :consts])))

(defmacro let-args
  [ast name-lookup argvec & body]
  (let [lookups (mapcat #(vector %
                              `(lookup-expr ~name-lookup ~%))
                     argvec)]
    `(let [{:keys ~argvec} (merged-args ~ast)
           ~@lookups]
       ~@body)))

(defmethod verilog-of :make-union
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [schema enum]} t
        {:keys [tag val]} (merged-args ast)
        w (bit-width-of t)
        padding (- w
                   (bit-width-of enum)
                   (bit-width-of (tag schema)))]
    (make-union-verilog
      (lookup-expr name-lookup (enum tag))
      padding
      (lookup-expr name-lookup val))))

(defmethod verilog-of :get-value
  [ast name-lookup]
  (let [valtype (typeof ast) 
        union (:u (merged-args ast))
        top  (dec (bit-width-of valtype))]
    (str (lookup-expr name-lookup union)
         "[" top (when-not (zero? top) ":0") "]"))) 

(defmethod verilog-of :get-tag
  [ast name-lookup]
  (let [enum (typeof ast) 
        union (:u (merged-args ast))
        top (dec (bit-width-of (typeof union)))
        bottom (inc (- top (bit-width-of enum)))]
    (str (lookup-expr name-lookup union)
         "[" top 
         (when-not (= top bottom)
           (str ":" bottom))
         "]"))) 

(defmethod verilog-of :make-bundle
  [ast name-lookup]
  (let [schema-ks (keys (:schema (typeof ast)))
        bundle-inst (merged-args ast)
        ordered-vals (map #(lookup-expr name-lookup
                                        (get bundle-inst %))
                          schema-ks)]
    (str "{" (join ", " ordered-vals) "}")))

(defn compute-key-offsets
  "Returns a map from keys to pairs. The pairs
  are the low (inclusive) to high (inclusive)
  bit indices in the verilog representation."
  [bundle-type]
  (let [schema (:schema bundle-type)
        key-widths (map (comp bit-width-of
                              (partial get schema))
                        (keys schema))
        offsets (reductions + (conj key-widths 0))
        total (reduce + key-widths)
        pairs (partition 2 1 (map (partial - total) offsets))
        pairs (map (fn [[x y]] [(dec x) y]) pairs)
        ]
    (into {} (map vector (keys schema) pairs))))

(defmethod verilog-of :bundle-assoc
  [ast name-lookup]
  (let [{:keys [bund k v]} (merged-args ast)
        t (typeof ast)
        offsets (compute-key-offsets t)
        w (bit-width-of t)
        [high low] (get offsets k)
        v (lookup-expr name-lookup v)
        bund (lookup-expr name-lookup bund)] 
    (str "{"
         (when-not (= (dec w) high)
           (str bund "[" (dec w) ":" (inc high) "], "))
         v
         (when-not (= low 0)
           (str ", " bund "[" (dec low) ":0]"))
         "}")))

(defmethod verilog-of :bundle-key
  [ast name-lookup]
  (let [{:keys [bund key]} (merged-args ast)
        offsets (compute-key-offsets (typeof bund))
        [high low] (get offsets key)]
    (str (lookup-expr name-lookup bund) "[" high ":" low "]")))

(defmethod verilog-of :slice
  [ast name-lookup]
  (let [{:keys [expr high low]} (merged-args ast)]
    (str (lookup-expr name-lookup expr) "[" (dec high) ":" low "]")))

(defmethod verilog-of :bit-cat
  [ast name-lookup]
  (let-args ast name-lookup [b1 b2]
    (str "{" b1 ", " b2 "}")))

(defmethod verilog-of :mux2
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [sel v1 v2]} (merged-args ast)]
    (str (lookup-expr name-lookup sel) " ? "
         (lookup-expr name-lookup v1) " : "
         (lookup-expr name-lookup v2))))

(defmethod verilog-of :+
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " + " rhs)))

(defmethod verilog-of :=
  [ast name-lookup]
   (let [{:keys [x y]} (merged-args ast)]
     (str (lookup-expr name-lookup x) " == " (lookup-expr name-lookup y))))

(defn verilog-noop-passthrough
  [ast name-lookup]
  (let [args (merged-args ast)]
    (when-not (contains? args :expr)
      (throw+ (error "must have an :expr" args)))
    (when (not= 1 (count args))
      (throw+ (error "must have exactly one expr")))
    (verilog-of (:expr args) name-lookup)))

(defmethod verilog-of :cast
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :noop
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :serialize
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))

(defn render-single-expr
  "Takes an expr and a name table and
  returns an updated name table and a string
  to add to the text of the verilog"
  [expr name-table]
  (cond
    (pipinst? expr)
    [(assoc name-table expr (verilog-repr expr)) ""]
    (= :use-core-impl (piplin-clojure-dispatch expr))
    (do
      (println "WARNING: trying to render clojure type, skipping")
      [name-table ""])
    ;The is common subexpression elimination
    (contains? name-table expr)
    [name-table ""]
    :else
    (let [name (name (gensym))
          indent "  "
          zero-offset-bit-width (dec (bit-width-of (typeof expr)))
          wire-decl (str "wire " (when (pos? zero-offset-bit-width)
                                   (str "[" zero-offset-bit-width ":0] ")))
          assign " = "
          body (verilog-of expr name-table)
          terminator ";\n"]
      [(assoc name-table expr name)
       (str indent
            wire-decl
            name
            assign
            body
            terminator)])))

(defn verilog
  "first, recurses to non-const members of the expr.
  then, renders all const members of the expr.
  finally, renders the expr itself.

  returns the updated name-table and text"
  ([expr name-table]
   (verilog expr name-table "")) 
  ([expr name-table text]
   (if (pipinst? expr)
     (render-single-expr expr name-table)
     (let [args (vals (:args (value expr)))
           [name-table text]
           (reduce
             (fn [[name-table text] expr]
               (verilog expr
                        name-table
                        text))
             [name-table text]
             args)
           consts (vals (:consts (value expr)))
           [name-table text]
           (reduce
             (fn [[name-table text] expr]
               (verilog expr
                        name-table
                        text))
             [name-table text]
             args)
           [name-table partial-text]
           (render-single-expr expr name-table)]
       [name-table (str text partial-text)]))))

(defn init-name-table [module-inst]
  (->> module-inst
    :ports
    map-invert
    (reduce (fn [accum [k v]] (assoc accum k (name v))) {})))

;ex: (verilog (+ ((uintm 3) 0) (uninst ((uintm 3) 1))) {})

;way more awesome that this works:
(comment
(def e (enum #{:a :b :c})) 
(def b (bundle {:car e :cdr (uintm 4)})) 
(def u (union {:x (uintm 5) :y b})) 
            
(-> (verilog (union-match (uninst (u {:x ((uintm 5) 0)}))
              (:x x (bit-slice (serialize x) 1 4))
              (:y {:keys [car cdr]} #b00_1)) {}) second print))
