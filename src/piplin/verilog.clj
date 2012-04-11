(ns piplin.verilog
  (:use [slingshot.slingshot])
  (:use [clojure.walk :only [postwalk]]) 
  (:use fleet) 
  (:use [clojure.string :only [join]]) 
  (:use [piplin math modules types]))


(def uintm-add-template
  (fleet [bits x y out]
         "uintm_add #(<(str bits)>)
           <(gensym \"uintm_add\")>(
             .x(<(str x)>),
             .y(<(str y)>),
             .out(<(str out)>));"))

;example
(str (uintm-add-template 1 2 3 4))

(defmulti verilog-repr
  kindof)
(defmethod verilog-repr :default
  [x]
  (throw+ (error "cannot convert to verilog:" x)))

(defmethod verilog-repr :uintm
  [x]
  (value x))

(defmethod verilog-repr :bits
  [x]
  (let [t (typeof x)
        b (value x)
        w (bit-width-of t)]
    (str w "'b" (join b))))

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

(defmethod verilog-of :+
  [ast name-lookup]
  (let [ast (value ast)
        args (apply merge (map ast [:args :consts]))]
    (str (get name-lookup (:lhs args))
         " + "
         (get name-lookup (:rhs args)))))

(defmethod verilog-of :noop
  [ast name-lookup]
  (let [args (apply merge (map (value ast) [:args :consts]))]
  (println args)
    (when-not (:expr args)
      (throw+ (error "noop must have an :expr" args)))
    (when (not= 1 (count args))
      (throw+ (error "noop must have exactly one expr")))
    (verilog-of (:expr args) {})))

(defn render-single-expr
  "Takes an expr and a name table and
  returns an updated name table and a string
  to add to the text of the verilog"
  [expr name-table]
  (let [name (name (gensym))
        indent "  "
        zero-offset-bit-width (dec (bit-width-of (typeof expr)))
        wire-decl (str "wire [" zero-offset-bit-width ":0] ")
        assign " = "
        body (if (pipinst? expr)
               (verilog-repr expr)
               (verilog-of expr name-table))
        terminator ";\n"]
    [(assoc name-table expr name)
     (str indent
         wire-decl
         name
         assign
         body
         terminator)]))

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

;ex: (verilog (+ ((uintm 3) 0) (uninst ((uintm 3) 1))) {})
