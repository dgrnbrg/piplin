(ns piplin.verilog
  (:refer-clojure :exclude [replace cast])
  (:require [piplin.walk :as walk])
  (:use [slingshot.slingshot])
  (:use [clojure.walk :only [postwalk]]) 
  (:use [clojure.set :only [map-invert]])
  (:use [clojure.string :only [join replace]]) 
  (:use [piplin modules types])
  (:use [piplin.types [bits :only [bit-width-of bits serialize]]])
  (:use [piplin.types :only [piplin-clojure-dispatch]]))

(defn sanitize-str
  "Takes a string and makes it safe for verilog."
  [s]
  (replace s \- \_))

(defn make-union-verilog
  [tag padding value]
  (str "{" tag
       (when (pos? padding)
         (str ", " padding "'b" 0)) 
       ", " value "}"))

(defmulti verilog-repr
  kindof
  :hierarchy types)
(defmethod verilog-repr :default
  [x]
  (throw+ (error "cannot convert to verilog:" x)))

;TODO: why must this method be included?
;I cannot figure out where it fits in the output
;probably used by bit-slice as a const, but not actually
;used so it doesn't appear in output
(defmethod verilog-repr :j-long
  [x]
  (str "j-long is unconvertible {" x \}))

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
    (if (zero? w)
      "0'b0"
      (str w "'b" (join b)))))

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
  (verilog-repr (serialize x)))

(defmethod verilog-repr :array
  [x]
  (verilog-repr (serialize x)))

(defn lookup-expr
  [table expr]
  (if (pipinst? expr)
    (verilog-repr expr)
    (if-let [name (get table expr)]
      name
      (do
        "UNKNOWN_QUANTITY"
       #_(throw+ (error expr "not found in" table))))))

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

(defn get-args
  [ast]
  (:args (value ast)))

(defmacro let-args
  [ast name-lookup argvec & body]
  (let [lookups (mapcat #(vector %
                              `(lookup-expr ~name-lookup ~%))
                     argvec)]
    `(let [{:keys ~argvec} (get-args ~ast)
           ~@lookups]
       ~@body)))

(defmethod verilog-of :make-union
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [schema enum]} t
        {:keys [tag val]} (get-args ast)
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
        union (:u (get-args ast))
        top  (dec (bit-width-of valtype))]
    (str (lookup-expr name-lookup union)
         "[" top (when-not (zero? top) ":0") "]"))) 

(defmethod verilog-of :get-tag
  [ast name-lookup]
  (let [enum (typeof ast) 
        union (:u (get-args ast))
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
        bundle-inst (get-args ast)
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
  (let [{:keys [bund k v]} (get-args ast)
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
  (let [{:keys [bund key]} (get-args ast)
        offsets (compute-key-offsets (typeof bund))
        [high low] (get offsets key)]
    (str (lookup-expr name-lookup bund) "[" high ":" low "]")))

(defmethod verilog-of :make-array
  [ast name-lookup]
  (let [keys (->> (typeof ast)
               :array-len
               range
               (map (comp keyword str)))
        array-inst (get-args ast)
        ordered-vals (map #(lookup-expr name-lookup
                                        (get array-inst %))
                          keys)]
    (str "{" (join ", " ordered-vals) "}")))

(defmethod verilog-of :array-get
  [ast name-lookup]
  (let [{:keys [array i]} (get-args ast)
        subtype-width (bit-width-of (:array-type (typeof array)))
        index (lookup-expr name-lookup i)]
    (str
      (lookup-expr name-lookup array)
      "[" 
      (lookup-expr name-lookup i)
      "]")))

(defmethod verilog-of :array-nth
  [ast name-lookup]
  (let [{:keys [array i]} (get-args ast)]
    (lookup-expr name-lookup (get array (keyword (str i))))))

#_(defmethod verilog-of :array-assoc
  [ast name-lookup]
  (let [{:keys [array index v]} (get-args ast)
        {:keys [array-len array-type]} (typeof array)
        upper-bound (dec (* (bit-width-of array-type) (inc index))) 
        lower-bound (* (bit-width-of array-type) index)
        array-sym (lookup-expr name-lookup array)
        arg-sym (lookup-expr name-lookup v)]
    (str "{"
         (when-not (= (dec (bit-width-of (typeof ast))) high)
           (str bund "[" (dec w) ":" (inc high) "], "))
         v
         (when-not (= low 0)
           (str ", " bund "[" (dec low) ":0]"))
         "}")))

(defmethod verilog-of :slice
  [ast name-lookup]
  (let [{:keys [expr high low]} (get-args ast)]
    (str (lookup-expr name-lookup expr) "[" (dec high) ":" low "]")))

(defmethod verilog-of :bit-cat
  [ast name-lookup]
  (let-args ast name-lookup [b1 b2]
    (str "{" b1 ", " b2 "}")))

(defmethod verilog-of :mux2
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [sel v1 v2]} (get-args ast)]
    (str (lookup-expr name-lookup sel) " ? "
         (lookup-expr name-lookup v1) " : "
         (lookup-expr name-lookup v2))))

(defmethod verilog-of :+
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " + " rhs)))

(defmethod verilog-of :-
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " - " rhs)))

(defmethod verilog-of :*
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " * " rhs)))

(defmethod verilog-of :>
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " > " rhs)))

(defmethod verilog-of :>=
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " >= " rhs)))

(defmethod verilog-of :<
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " - " rhs)))

(defmethod verilog-of :<=
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " <= " rhs)))

(defmethod verilog-of :bit-and
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " & " rhs)))

(defmethod verilog-of :bit-or
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " | " rhs)))

(defmethod verilog-of :bit-xor
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            (str lhs " ^ " rhs)))

(defmethod verilog-of :bit-not
  [ast name-lookup]
  (let-args ast name-lookup [x]
            (str "~" x)))

(defmethod verilog-of :=
  [ast name-lookup]
   (let [{:keys [x y]} (get-args ast)]
     (str (lookup-expr name-lookup x) " == " (lookup-expr name-lookup y))))

(defn verilog-noop-passthrough
  [ast name-lookup]
  (let [args (get-args ast)]
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

(defn array-width-decl [x]
  (if (zero? x)
    ""
    (str "[" (dec x) ":0]")))

(defn render-single-expr
  "Takes an expr and a name table and
  returns an updated name table and a string
  to add to the text of the verilog"
  [expr name-table]
  (cond
    (pipinst? expr)
    [nil ""]
    (= :use-core-impl (piplin-clojure-dispatch expr))
    (do
      (println "WARNING: trying to render clojure type, skipping")
      [nil ""])
    :else
    (let [name (name (gensym))
          indent "  "
          bit-width (bit-width-of (typeof expr))
          wire-decl (str "wire " (array-width-decl bit-width) " ")
          assign " = "
          body (verilog-of expr name-table)
          terminator ";\n"]
      [name
       (str indent
            wire-decl
            name
            assign
            body
            terminator)])))

;todo this can take a long time
(defn verilog
  ([expr name-table]
   (verilog expr name-table ""))
  ([expr name-table text]
   (let [[name-table body]
         (walk/compile expr render-single-expr
                       name-table [])]
     [name-table (str text (join body))]))) 

(defn module-decl
  "Declares a module, using a map from strings
  to strings to populate the connections."
  [decl-name module connections]
  (str "  " (sanitize-str (name (:token module))) " " decl-name "(\n"
       "    .clock(clock), .reset(reset)" (when (seq connections) \,)
       "\n"
       (join ",\n"
         (map (partial str "    ")
                  (map (fn [[port conn]]
                         (str \. port \( conn \)))
                       connections)))
       "\n"
       "  );\n"))

(defn init-name-table [module-inst]
  (let [module-ports (->> module-inst
                       :ports
                       (reduce (fn [accum port]
                                 (let [{port-kw :port} (value port)]
                                   (assoc accum port (name port-kw))))
                               {}))
        ;We must find all the submodule port
        ;references to add to the name-table
        module-exprs (walk-connects module-inst
                                    #(get-in % [:args :expr])
                                    concat)
        subports (mapcat (fn [expr]
                        (walk-expr expr
                          (fn [expr]
                              (if (= (:port-type (value expr))
                                    :subport)
                               [expr] nil))
                          concat)) module-exprs)
        subports-map (->> (set subports)
                       (mapcat (fn [{:keys [module port] :as subport}]
                                 [subport (str (name module)
                                               \.
                                               (name port))]))
                       (apply hash-map))]
    (merge subports-map module-ports)))

(defn module->verilog
  [module]
  (when-not (= (:type module) :module)
    (throw+ (error module "must be a module")))
  (let [ports  (mapcat (comp keys #(get module %)) [:inputs :outputs])
        inputs (->> (:inputs module)
                 (map (fn [[k v]] [(name k) (bit-width-of v)]))
                 (into {})) 
        outputs (->> (:outputs module)
                  (map (fn [[k v]]
                         (if (= :array (kindof v))
                           [(name k)
                            (bit-width-of (:array-type (typeof v)))
                            (:array-len (typeof v))]
                           [(name k)
                            (bit-width-of (typeof v))
                            nil]))))
        feedbacks (->> (:feedback module)
                    (map (fn [[k v]]
                           (if (= :array (kindof v))
                             [(name k)
                              (bit-width-of (:array-type (typeof v)))
                              (:array-len (typeof v))]
                             [(name k)
                              (bit-width-of (typeof v))
                              nil]))))
        initials (->> (merge (:outputs module) (:feedback module))
                   (mapcat (fn [[k v]]
                             (if-not (= :array (kindof v))
                               [[(name k) (verilog-repr v)]]
                               (map (fn [index v]
                                      [(str (name k) \[ index \])
                                       (verilog-repr v)])
                                    (range) v)))))
        submodules (->> (:modules module)
                     (map (fn make-module-decls [[k v]]
                            (module-decl (name k) v {}))))
        name-table (init-name-table module)
        connections (->> module
                      :body
                      (filter #(= :register (:type %)))
                      (map (comp :args value))
                      (map (fn [{:keys [reg expr] :as r}]
                             (let [index-expr (let [reg (value reg)]
                                                (when (= (:op reg) :array-get)
                                                  (get-in reg [:args :i])))
                                   reg (if-not index-expr reg
                                         (-> reg value :args :array))]
                               [(-> reg value :port name)
                                expr
                                index-expr]))))
        input-connections (->> module
                      :body
                      (filter #(= (:type %) :subport))
                      (map (comp :args value))
                      (map (fn [{:keys [reg expr]}]
                               [(->> reg value ((juxt :module :port)) (map name) (join \.))
                                expr])))
        [name-table body] (reduce
                            (fn [[name-table text] expr]
                              (verilog expr name-table text))
                            [name-table ""] (concat
                                              (map second input-connections)
                                              (filter identity
                                               (mapcat next connections))))
        ]
    (str "module " (sanitize-str (name (:token module))) "(\n"
         "  clock,\n"
         "  reset,\n"
         (join ",\n" (map #(str "  " (name %)) ports)) "\n"
         ");\n"
         "  input wire clock;\n"
         "  input wire reset;\n"
         (join "\n" submodules)
         (when (seq inputs)
           (str "\n"
                "  //inputs\n"
                (join (map (fn [[input width]]
                             (str "  input wire " (array-width-decl width) " " input ";\n"))
                           inputs))))
         (when (seq outputs)
           (str "\n"
                "  //outputs\n"
                (join (map (fn [[output width len]]
                             (if-not len
                               (str "  output reg " (array-width-decl width) " " output ";\n")
                               (str "  output reg " (array-width-decl width) " " output " " (array-width-decl len) ";\n")) 
                             )
                           outputs))))
         (when (seq feedbacks)
           (str "\n"
                "  //feedback\n"
                (join (map (fn [[feedback width len]]
                             (if-not len
                               (str "  reg " (array-width-decl width) " " feedback ";\n")
                               (str "  reg " (array-width-decl width) " " feedback " " (array-width-decl len) ";\n")))
                           feedbacks))))
         "\n"
         "\n"
         body
         "\n"
         "\n"
         "  always @(posedge clock)\n"
         "    if (reset) begin\n"
         (join (map (fn [[name val]]
                      (str "      " name " <= " val ";\n"))
                    initials))
         "    end else begin\n"
         (join (map (fn [[name expr index-expr]]
                      (str "      " name
                           (when index-expr
                             (str \[ (lookup-expr name-table index-expr) \]))
                           " <= " (lookup-expr name-table expr) ";\n"))
                    connections))
         "    end\n"
         "\n"
         (join (map (fn [[name expr]]
                      (str "  assign " name " = " (lookup-expr name-table expr) ";\n"))
                    input-connections))
         "endmodule\n")))

(defn regs-for-inputs
  [module]
  [(map #(let [w (-> % val bit-width-of)]
           (str "reg "
                (array-width-decl w)
                " "
                (-> % key name)
                " = " w "'b" (join (repeat w \z)) ";\n"))
        (:inputs module))
   (map #(let [n (-> % key name)] (str \. n \( n \)))
        (:inputs module))])

(defn wire-for-regs
  [module]
  [[]
   (map #(str \. (-> % key name) "()")
        (mapcat #(get module %) [:outputs]))]) ;TODO: should this include :feedback?

(defn assert-hierarchical
  [indent dut-name var val]
  (let [dut-var (str dut-name \. var) 
        assertion-str (str dut-var " !== " val)]
    (str indent "if (" assertion-str ") begin\n"
         indent "  $display(\"failed assertion: " assertion-str ", instead is %b\", " dut-var ");\n"
         indent "  $finish;\n"
         indent "end\n")))

(defn assert-hierarchical-cycle
  "indent is prepended as indentation (usually whitespace).
  
  dut-name is the name of the dut module; registers will
  be asserted hierarchically rooted at the dut.
  
  cycle-map is a map from vectors of keywords representing
  the hierarchical path to a register (excluding the dut's name)
  to the values."
  [indent dut-name cycle-map]
  (reduce (fn [text [path val]]
            (when-not (vector? path)
              (throw+ "Path must be a vector"))
            (let [array? (= :array (kindof val))]
              (str text
                   (if-not array?
                     (assert-hierarchical indent dut-name
                                          (join \. (map name path))
                                          (verilog-repr val))
                     (->> (map (fn [val index]
                                 (assert-hierarchical
                                   indent dut-name
                                   (str (join \. (map name path))
                                        \[ index \])
                                   (verilog-repr val)))
                               val (range))
                       (join "\n"))))))
          ""
          cycle-map))

(defn make-testbench
  "Produces verilog that will run a test
  against a given module. samples is a seqable
  of maps, where the keys are all the inputs
  and registers of the module, and the simulation
  is run with those inputs and asserting those
  states. The generated verilog will do this check,
  and generate the appropriate clock and reset signanls."
  [module samples]
  (let [[input-decls input-connects] (regs-for-inputs module)
        [output-decls output-connects] (wire-for-regs module)
        dut-name "dut"]
    (str
      "module test;\n"
      "  reg clk = 0;\n"
      "  always #5 clk = !clk;\n"
      "  reg rst = 1;\n"
      "\n"
      (join (map (partial str "  ") input-decls))
      (join (map (partial str "  ") output-decls))
      "  " (sanitize-str (name (:token module))) " " dut-name "(\n"
      "    .clock(clk), .reset(rst)" (when
                                       (->
                                         (concat
                                           input-connects
                                           output-connects)
                                         seq) \,)
      "\n"
      (join (map (partial str "    ")
                 (concat input-connects output-connects))) "\n"
      "  );\n"
      "\n"
      "  initial begin\n"
      ;"    $dumpfile(\"dump.vcd\");\n"
      ;"    $dumpvars(0);\n"
      "    #10 rst = 0;\n"
      (->> samples
        
        (map #(assert-hierarchical-cycle "    "
                                         dut-name
                                         %))
        (map #(str % "    #10\n"))
        join)
      "    $display(\"test passed\");\n"
      "    $finish;\n"
      "  end\n"
      "endmodule")))

(defn modules->verilog
  "This takes a module (the root module),
  and returns a list of pairs where the first
  element is a module, the second element is
  that module's verilog."
  [root]
  (walk-modules root
    (fn visit [module]
      [[module (module->verilog module)]])
    (fn combine [x y]
      (let [left-modules (set (map first x))]
        (if (left-modules (ffirst y))
          x
          (concat x y))))))

(defn modules->all-in-one
  "Takes a root module and returns a string
  which is a single verilog file with the
  entire module hierarchy included."
  [root]
  (->> (modules->verilog root)
    (map second)
    (join "\n")))

;ex: (verilog (+ ((uintm 3) 0) (uninst ((uintm 3) 1))) {})

;way more awesome that this works:
(comment
  (def e (enum #{:a :b :c} ))
  (def b (bundle {:car e :cdr (uintm 4)})) 
  (def u (union {:x (uintm 5) :y b})) 

  (-> (verilog (union-match (uninst (u {:x ((uintm 5) 0)}))
  (:x x (bit-slice (serialize x) 1 4))
  (:y {:keys [car cdr]} #b00_1)) {}) second print))

(defn modules->verilog+testbench
  [mod cycles]
  (str (modules->all-in-one mod)
       "\n"
       (make-testbench mod (trace-module mod cycles))))
