(ns piplin.verilog
  (:use [slingshot.slingshot])
  (:use [clojure.walk :only [postwalk]]) 
  (:use fleet) 
  (:use [clojure.set :only [map-invert]])
  (:use [clojure.string :only [join]]) 
  (:use [piplin modules types])
  (:use [piplin [math :only [bit-width-of]]]))


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

(defn lookup-expr
  [table expr]
  (if (pipinst? expr)
    (verilog-repr expr)
    (if-let [name (get table expr)]
      name
      (throw+ (error expr "not found in" table)))))

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

(defmethod verilog-of :+
  [ast name-lookup]
  (let [ast (value ast)
        args (apply merge (map ast [:args :consts]))]
    (str (lookup-expr name-lookup (:lhs args))
         " + "
         (lookup-expr name-lookup (:rhs args)))))

(defmethod verilog-of :make-union
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [schema enum]} t
        {:keys [tag val]} (apply merge (map (value ast) [:args :consts]))
        w (bit-width-of t)
        lolol (println (str "enum " enum))
        lolol (println (str "tag " tag))
        lolol (println (str "schema " schema))
        lolol (println (str "tag schema " (get schema tag)))
        lolol (println (str "val " val))
        padding (- w
                   (bit-width-of enum)
                   (bit-width-of (tag schema)))]
    (make-union-verilog
      (lookup-expr name-lookup (enum tag))
      padding
      (lookup-expr name-lookup val))))

(defn verilog-noop-passthrough
  [ast name-lookup]
  (let [args (apply merge (map (value ast) [:args :consts]))]
    (when-not (:expr args)
      (throw+ (error "cast must have an :expr" args)))
    (when (not= 1 (count args))
      (throw+ (error "cast must have exactly one expr")))
    (verilog-of (:expr args) name-lookup)))

(defmethod verilog-of :cast
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :noop
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))

(defn render-single-expr
  "Takes an expr and a name table and
  returns an updated name table and a string
  to add to the text of the verilog"
  [expr name-table]
  (pprint-ast ["rendering expr: " expr])
  (cond
    (pipinst? expr)
    [(assoc name-table expr (verilog-repr expr)) ""]
    ;The is common subexpression elimination
    (contains? name-table expr)
    [name-table ""]
    :else
    (let [name (name (gensym))
          indent "  "
          zero-offset-bit-width (dec (bit-width-of (typeof expr)))
          wire-decl (str "wire [" zero-offset-bit-width ":0] ")
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
   (pprint-ast expr)
   (verilog expr name-table "")) 
  ([expr name-table text]
   (if (pipinst? expr)
     (render-single-expr expr name-table)
     (let [args (vals (:args (value expr)))
     ;      lolol (pprint-ast ["args: " args])
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
