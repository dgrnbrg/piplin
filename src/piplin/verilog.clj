(ns piplin.verilog
  (:refer-clojure :exclude [replace cast])
  (:require [piplin.walk :as walk])
  (:use [slingshot.slingshot])
  (:use [clojure.walk :only [postwalk]]) 
  (:use [clojure.set :only [map-invert]])
  (:use [clojure.string :only [join replace]]) 
  (:use [piplin modules types])
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types.sints :only [sign-extend sints]])
  (:use [piplin.types [bits :only [bit-width-of bits deserialize serialize]]])
  (:use [piplin [types :only [piplin-clojure-dispatch]] protocols]))

(defn sanitize-str
  "Takes a string and makes it safe for verilog."
  [s]
  (replace (munge (name s)) "." "_DOT_"))

(declare render-single-expr)

(defn array-width-decl
  "Takes a width `x` and returns the string `\"[x:0]\"`,
  or `\"\"` if x is one"
  [x]
  (if (= 1 x)
    ""
    (str "[" (dec x) ":0]")))

(defn format-verilog
  "Takes a wire name, its width, and the corresponding
  verilog, and returns the formatted `wire` declaration."
  [width name verilog & args]
  (format "  wire %s %s = %s;\n"
          (array-width-decl width) name (apply format verilog args)))

(defn gen-verilog-name
  "Takes a string, symbol, or keyword, and returns a unique
  similar verilog name."
  ([] (name (gensym)))
  ([base-name]
   (-> base-name
     name
     sanitize-str
     gensym
     name)))

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
  (throw+ (error "cannot convert " (kindof x) " to verilog:" x)))

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

(defmethod verilog-repr :sints 
  [x]
  (let [t (typeof x)
        i (value x)
        w (bit-width-of t)] 
    (str (when (neg? i) "-")
         w "'d"
         (if (neg? i) (- i) i))))

(defmethod verilog-repr :sfxpts
  [x]
  (verilog-repr (serialize x)))

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
        #_"UNKNOWN_QUANTITY"
       (throw+ (error expr "not found in" table))))))

(defmulti verilog-of
  (fn [ast name-lookup] (if (pipinst? ast)
                          ::immediate
                          (:op (value ast)))))

(defmethod verilog-of :default
  [ast name-lookup]
  (throw+ (error "cannot convert" (:op (value ast)) "to verilog:" ast)))

(defmethod verilog-of ::immediate
  [ast name-lookup]
  [(verilog-repr ast)])

(defmethod verilog-of :port
  [ast name-lookup]
  [(lookup-expr name-lookup ast)])

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
    [(make-union-verilog
      (lookup-expr name-lookup (enum tag))
      padding
      (lookup-expr name-lookup val))]))

(defmethod verilog-of :get-value
  [ast name-lookup]
  (let [valtype (typeof ast) 
        union (:u (get-args ast))
        top  (dec (bit-width-of valtype))]
    [(str (lookup-expr name-lookup union)
         "[" top (when-not (zero? top) ":0") "]")])) 

(defmethod verilog-of :get-tag
  [ast name-lookup]
  (let [enum (typeof ast) 
        union (:u (get-args ast))
        top (dec (bit-width-of (typeof union)))
        bottom (inc (- top (bit-width-of enum)))]
    [(str (lookup-expr name-lookup union)
         "[" top 
         (when-not (= top bottom)
           (str ":" bottom))
         "]")])) 

(defmethod verilog-of :make-bundle
  [ast name-lookup]
  (let [schema-ks (keys (:schema (typeof ast)))
        bundle-inst (get-args ast)
        ordered-vals (map #(lookup-expr name-lookup
                                        (get bundle-inst %))
                          schema-ks)]
    [(str "{" (join ", " ordered-vals) "}")]))

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
    [(str "{"
         (when-not (= (dec w) high)
           (str bund "[" (dec w) ":" (inc high) "], "))
         v
         (when-not (= low 0)
           (str ", " bund "[" (dec low) ":0]"))
         "}")]))

(defmethod verilog-of :bundle-key
  [ast name-lookup]
  (let [{:keys [bund key]} (get-args ast)
        offsets (compute-key-offsets (typeof bund))
        [high low] (get offsets key)]
    [(str (lookup-expr name-lookup bund) "[" high ":" low "]")]))

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
    [(str "{" (join ", " ordered-vals) "}")]))

(defmethod verilog-of :array-get
  [ast name-lookup]
  (let [{:keys [array i]} (get-args ast)
        subtype-width (bit-width-of (:array-type (typeof array)))
        index (lookup-expr name-lookup i)]
    [(str
      (lookup-expr name-lookup array)
      "[" 
      (lookup-expr name-lookup i)
      "]")]))

(defmethod verilog-of :array-nth
  [ast name-lookup]
  (let [{:keys [array i]} (get-args ast)]
    [(lookup-expr name-lookup (get array (keyword (str i))))]))

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
    [(str (lookup-expr name-lookup expr) "[" (dec high) ":" low "]")]))

(defmethod verilog-of :bit-cat
  [ast name-lookup]
  (let-args ast name-lookup [b1 b2] 
    (let [{b1-ast :b1 b2-ast :b2} (get-args ast)
          b1-zero? (zero? (-> b1-ast typeof bit-width-of))
          b2-zero? (zero? (-> b2-ast typeof bit-width-of))]
      (cond
        (and b1-zero? b2-zero?)
        (throw+ (error "Cannot concat 2 zero-width signals"))
        b1-zero? [b2]
        b2-zero? [b1]
        :else
        [(str "{" b1 ", " b2 "}")]))))

(defmethod verilog-of :mux2
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [sel v1 v2]} (get-args ast)]
    [(str (lookup-expr name-lookup sel) " ? "
         (lookup-expr name-lookup v1) " : "
         (lookup-expr name-lookup v2))]))

(defmethod verilog-of :sign-extend
  [ast name-lookup]
  (let [orig-width (-> ast
                     value
                     :args
                     :num
                     typeof
                     bit-width-of)
        new-width (-> ast
                    typeof
                    bit-width-of)]
    (let-args ast name-lookup [num]
      [(format "{ {%d{%s[%d]}}, %s}"
               (- new-width orig-width)
               num (dec orig-width) num)])))

(defmethod verilog-of :+
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
    (condp = (kindof ast)
      :uintm
      [(str lhs " + " rhs)]
      :sintm
      [(str "$signed(" lhs ") + $signed(" rhs ")")]
      :sfxpts
      (let [{:as type
             :keys [i f]} (typeof ast)
            width (bit-width-of type)
            {:keys [lhs rhs]} (get-args ast)
            lhs-sints (->> (serialize lhs)
                        (deserialize (sints width)))
            rhs-sints (->> (serialize rhs)
                        (deserialize (sints width)))] 
        (verilog-of (impl/+ lhs-sints
                            rhs-sints)
                    (assoc name-lookup
                           lhs-sints (name-lookup lhs)
                           rhs-sints (name-lookup rhs)))
        )
      :sints
      (let [type (typeof ast)
            width (bit-width-of type)
           
            lhs-tmp-name (gen-verilog-name "lhs")
            lhs-tmp (format-verilog width lhs-tmp-name lhs)
            rhs-tmp-name (gen-verilog-name "rhs")
            rhs-tmp (format-verilog width rhs-tmp-name rhs)

            extended-sum-name (gen-verilog-name "extended_sum")
            extended-sum (format-verilog
                           (inc width)
                           extended-sum-name
                           "{%s[%d],%s} + {%s[%d],%s}"
                           lhs-tmp-name (dec width) lhs-tmp-name
                           rhs-tmp-name (dec width) rhs-tmp-name)

            extended-sum-msb (str extended-sum-name "[" width "]")
            overflow? (str "(" extended-sum-msb " == "
                           extended-sum-name "[" (dec width) "])")

            overflow-case (str "(" extended-sum-msb " == 1'b0) ? "
                               (verilog-repr
                                 (piplin.types.sints/max-value type))
                               " : "
                               (verilog-repr
                                 (piplin.types.sints/min-value type)))]
        [(str overflow? " ? "
              extended-sum-name (array-width-decl width)
              " : " overflow-case)
         [lhs-tmp rhs-tmp extended-sum]])
      (throw+ (error "Cannot add" (kindof ast))))))

(defmethod verilog-of :-
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
    (condp = (kindof ast)
      :uintm
      [(str lhs " - " rhs)]
      )))

(defmethod verilog-of :*
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
    (condp = (kindof ast)
      :uintm
      [(str lhs " * " rhs)]
      :sfxpts
      (let [{:as type
             :keys [i f]} (typeof ast)
            width (bit-width-of type)
            {:keys [lhs rhs]} (get-args ast)
            lhs-sints (->> (serialize lhs)
                        (deserialize (sints width))
                        (sign-extend (* 2 width)))
            rhs-sints (->> (serialize rhs)
                        (deserialize (sints width))
                        (sign-extend (* 2 width)))
            sints-* (impl/* lhs-sints rhs-sints) 
            [name-lookup' body] (walk/compile
                                  sints-*
                                  render-single-expr
                                  name-lookup [])

            sints-tmp-name (gen-verilog-name "full_result")
            sints-tmp (format-verilog
                        (* 2 width)
                        sints-tmp-name
                        (name-lookup' sints-*))]
        [(format "%s[%d:%d]"
                 sints-tmp-name
                 (+ f (dec width))
                 f)
         (conj (vec body) sints-tmp)])
      :sints
      (let [type (typeof ast)
            width (bit-width-of type)

            lhs-tmp-name (gen-verilog-name "lhs")
            lhs-tmp (format-verilog width lhs-tmp-name lhs)
            rhs-tmp-name (gen-verilog-name "rhs")
            rhs-tmp (format-verilog width rhs-tmp-name rhs)

            prod-sym (gen-verilog-name "prod")
            prod (format-verilog
                   width
                   prod-sym
                   "%s * %s"
                   lhs-tmp-name rhs-tmp-name) 

            lhs-pos?-sym (gen-verilog-name "lhs_pos") 
            rhs-pos?-sym (gen-verilog-name "rhs_pos") 
            lhs-pos? (format-verilog 1 lhs-pos?-sym "~%s[%d]" lhs-tmp-name (dec width))
            rhs-pos? (format-verilog 1 rhs-pos?-sym "~%s[%d]" rhs-tmp-name (dec width))

            lhs-leading-0s-sym (gen-verilog-name "lhs_leading_zeros")
            lhs-leading-0s-clauses (->> (range (dec width))
                                     (map #(format "~|%s[%d:%d] ? %d :"
                                                   lhs-tmp-name 
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            lhs-leading-0s (format-verilog
                             (log2 width) lhs-leading-0s-sym
                             lhs-leading-0s-clauses)
            rhs-leading-0s-sym (gen-verilog-name "rhs_leading_zeros")
            rhs-leading-0s-clauses (->> (range (dec width))
                                     (map #(format "~|%s[%d:%d] ? %d :"
                                                   rhs-tmp-name 
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            rhs-leading-0s (format-verilog
                             (log2 width) rhs-leading-0s-sym
                             rhs-leading-0s-clauses)

            lhs-leading-1s-sym (gen-verilog-name "lhs_leading_ones")
            lhs-leading-1s-clauses (->> (range (dec width))
                                     (map #(format "&%s[%d:%d] ? %d :"
                                                   lhs-tmp-name 
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            lhs-leading-1s (format-verilog
                             (inc (log2 width)) lhs-leading-1s-sym
                             lhs-leading-1s-clauses)
            rhs-leading-1s-sym (gen-verilog-name "rhs_leading_ones")
            rhs-leading-1s-clauses (->> (range (dec width))
                                     (map #(format "&%s[%d:%d] ? %d :"
                                                   rhs-tmp-name 
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            rhs-leading-1s (format-verilog 
                             (inc (log2 width)) rhs-leading-1s-sym
                             rhs-leading-1s-clauses)

            pos-pos-ovf?-sym (gen-verilog-name "pos_pos_ovf")
            pos-pos-ovf? (format-verilog
                           1 pos-pos-ovf?-sym
                           "(%s + %s < %d) | ((%s + %s == %d) & %s[%d])"

                           lhs-leading-0s-sym rhs-leading-0s-sym width
                           lhs-leading-0s-sym rhs-leading-0s-sym width
                           prod-sym (dec width))

            neg-neg-ovf?-sym (gen-verilog-name "neg_neg_ovf")
            neg-neg-ovf? (format-verilog
                           1 neg-neg-ovf?-sym
                           "(%s + %s < %d) | ((%s + %s == %d || %s + %s == %d) & (%s[%d] || %s == %s))"
                           lhs-leading-1s-sym rhs-leading-1s-sym width
                           lhs-leading-1s-sym rhs-leading-1s-sym width
                           lhs-leading-1s-sym rhs-leading-1s-sym (inc width)
                           prod-sym (dec width)
                           prod-sym (verilog-repr
                                      ((piplin.types.bits/bits width)
                                         (vec (repeat width 0)))))

            negative-leading-sym (gen-verilog-name "neg_leading")
            negative-leading (format-verilog
                               (inc (log2 width)) negative-leading-sym
                               "~%s ? %s : %s"
                               lhs-pos?-sym
                               lhs-leading-1s-sym rhs-leading-1s-sym)
            positive-leading-sym (gen-verilog-name "pos_leading")
            positive-leading (format-verilog
                               (log2 width) positive-leading-sym
                               "~%s ? %s : %s"
                               lhs-pos?-sym
                               rhs-leading-0s-sym lhs-leading-0s-sym)
            neg-pos-ovf?-sym (gen-verilog-name "neg_pos_ovf")
            neg-pos-ovf? (format-verilog
                           1 neg-pos-ovf?-sym
                           "(%s + %s < %d) | ~%s[%d]"
                           negative-leading-sym positive-leading-sym
                           width prod-sym (dec width))

            zero-sym (format "%d'd0" width)
            has-zero?-sym (gen-verilog-name "zero")
            has-zero? (format-verilog
                        1 has-zero?-sym
                        "%s == %s || %s == %s"
                        lhs-tmp-name zero-sym
                        rhs-tmp-name zero-sym)

            min-val (verilog-repr
                      (piplin.types.sints/min-value type))
            max-val (verilog-repr
                      (piplin.types.sints/max-value type))
            ]
        [(format "%s ? %s : ((%s & %s & %s) | (~%s & ~%s & %s)) ? %s : ((%s ^ %s) & %s) ? %s : %s"
                 has-zero?-sym zero-sym
                 lhs-pos?-sym rhs-pos?-sym pos-pos-ovf?-sym
                 lhs-pos?-sym rhs-pos?-sym neg-neg-ovf?-sym
                 max-val
                 lhs-pos?-sym rhs-pos?-sym neg-pos-ovf?-sym
                 min-val
                 prod-sym
                 )
         [lhs-tmp rhs-tmp has-zero? prod lhs-pos? rhs-pos? lhs-leading-0s lhs-leading-1s rhs-leading-0s rhs-leading-1s
          pos-pos-ovf? neg-neg-ovf? negative-leading positive-leading neg-pos-ovf?]]
        ))))

(defmethod verilog-of :>
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " > " rhs)]))

(defmethod verilog-of :>=
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " >= " rhs)]))

(defmethod verilog-of :<
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " - " rhs)]))

(defmethod verilog-of :<=
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " <= " rhs)]))

(defmethod verilog-of :bit-and
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " & " rhs)]))

(defmethod verilog-of :bit-or
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " | " rhs)]))

(defmethod verilog-of :bit-xor
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " ^ " rhs)]))

(defmethod verilog-of :bit-not
  [ast name-lookup]
  (let-args ast name-lookup [x]
            [(str "~" x)]))

(defmethod verilog-of :not
  [ast name-lookup]
  (let-args ast name-lookup [x]
    [(str "~" x)]))

(defmethod verilog-of :and
  [ast name-lookup]
  (let-args ast name-lookup [x y]
    [(str x " & " y)]))

(defmethod verilog-of :or
  [ast name-lookup]
  (let-args ast name-lookup [x y]
    [(str x " | " y)]))

(defmethod verilog-of :=
  [ast name-lookup]
   [(let [{:keys [x y]} (get-args ast)]
     (str (lookup-expr name-lookup x) " == " (lookup-expr name-lookup y)))])

(defn verilog-noop-passthrough
  ([ast name-lookup]
   (verilog-noop-passthrough ast name-lookup :expr)
   )
  ([ast name-lookup passthrough]
   (let [args (get-args ast)]
     (when-not (contains? args passthrough)
       (throw+ (error (str "must have an " passthrough)
                      args)))
     (when (not= 1 (count args))
       (throw+ (error "must have exactly one expr")))
     (verilog-of (passthrough args) name-lookup))))

(defmethod verilog-of :cast
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :noop
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :serialize
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :deserialize
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup :bits))

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
    (let [name (gen-verilog-name)
          bit-width (bit-width-of (typeof expr))
          [body structural] (verilog-of expr name-table)]
      [name
       (conj (vec structural)
             (format-verilog bit-width name body))])))

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
                                   (assoc accum port (sanitize-str port-kw))))
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
                                 [subport (str (sanitize-str module)
                                               \_
                                               (sanitize-str port))]))
                       (apply hash-map))]
    (merge subports-map module-ports)))

(defn module->verilog
  [module]
  (when-not (= (:type module) :module)
    (throw+ (error module "must be a module")))
  (let [ports  (mapcat (comp keys #(get module %)) [:inputs :outputs])
        inputs (->> (:inputs module)
                 (map (fn [[k v]] [(sanitize-str k) (bit-width-of v)]))
                 (into {})) 
        outputs (->> (:outputs module)
                  (map (fn [[k v]]
                         (if (= :array (kindof v))
                           [(sanitize-str k)
                            (bit-width-of (:array-type (typeof v)))
                            (:array-len (typeof v))]
                           [(sanitize-str k)
                            (bit-width-of (typeof v))
                            nil]))))
        feedbacks (->> (:feedback module)
                    (map (fn [[k v]]
                           (if (= :array (kindof v))
                             [(sanitize-str k)
                              (bit-width-of (:array-type (typeof v)))
                              (:array-len (typeof v))]
                             [(sanitize-str k)
                              (bit-width-of (typeof v))
                              nil]))))
        initials (->> (merge (:outputs module) (:feedback module))
                   (mapcat (fn [[k v]]
                             (if-not (= :array (kindof v))
                               [[(sanitize-str k)
                                 (verilog-repr v)]]
                               (map (fn [index v]
                                      [(str (sanitize-str k)
                                            \[ index \])
                                       (verilog-repr v)])
                                    (range) v)))))
        submodules (->> (:modules module)
                     (map (fn make-module-decls [[k v]]
                            (let [all-input-ports
                                  (->> v
                                    :inputs
                                    (map (fn [[k v]]
                                           [(sanitize-str k)
                                            v])))
                                  all-output-ports
                                  (->> v
                                    :outputs
                                    (map (fn [[k v]]
                                           [(sanitize-str k)
                                            (typeof v)])))
                                  module-name (sanitize-str k)
                                  all-ports (concat all-input-ports
                                                    all-output-ports)
                                  all-ports-wires
                                  (map (fn [[port type]]
                                         (str "  wire " (array-width-decl (bit-width-of type)) " " module-name \_ port ";\n")) all-ports)
                                  port-map
                                  (into
                                    {}
                                    (map (comp
                                           (juxt
                                             identity
                                             (partial str
                                                      module-name
                                                      \_))
                                           first)
                                         all-ports))]
                              (str
                                (join all-ports-wires)
                                (module-decl
                                  (sanitize-str k) v
                                  port-map))))))
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
                               [(-> reg value :port sanitize-str)
                                expr
                                index-expr]))))
        input-connections (->> module
                      :body
                      (filter #(= (:type %) :subport))
                      (map (comp :args value))
                      (map (fn [{:keys [reg expr]}]
                               [(->> reg value ((juxt :module :port)) (map sanitize-str) (join \_))
                                expr])))
        [name-table body] (reduce
                            (fn [[name-table text] expr]
                              (verilog expr name-table text))
                            [name-table ""] (concat
                                              (map second input-connections)
                                              (filter identity
                                               (mapcat next connections))))
        ]
    (str "module " (sanitize-str (:token module)) "(\n"
         "  clock,\n"
         "  reset,\n"
         (join ",\n" (map #(str "  " (sanitize-str %)) ports)) "\n"
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
   (map #(str \. (-> % key sanitize-str) "()")
        (mapcat #(get module %) [:outputs]))]) ;TODO: should this include :feedback?

(defn assert-hierarchical
  [indent dut-name var val cycle]
  (let [dut-var (str dut-name \. var) 
        assertion-str (str dut-var " !== " val)]
    (str indent "if (" assertion-str ") begin\n"
         indent "  $display(\"Cycle " cycle ": failed assertion: " assertion-str ", instead is %b\", " dut-var ");\n"
         indent "  $finish;\n"
         indent "end\n")))

(defn assert-hierarchical-cycle
  "indent is prepended as indentation (usually whitespace).
  
  dut-name is the name of the dut module; registers will
  be asserted hierarchically rooted at the dut.
  
  cycle-map is a map from vectors of keywords representing
  the hierarchical path to a register (excluding the dut's name)
  to the values."
  [indent dut-name cycle-map cycle]
  (reduce (fn [text [path val]]
            (when-not (vector? path)
              (throw+ "Path must be a vector"))
            (let [array? (= :array (kindof val))]
              (str text
                   (if-not array?
                     (assert-hierarchical indent dut-name
                                          (join \. (map sanitize-str path))
                                          (verilog-repr val)
                                          cycle)
                     (->> (map (fn [val index]
                                 (assert-hierarchical
                                   indent dut-name
                                   (str (join \. (map sanitize-str path))
                                        \[ index \])
                                   (verilog-repr val)
                                   cycle))
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
      "  " (sanitize-str (:token module)) " " dut-name "(\n"
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
                                         %2 %1)
             (range))
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
