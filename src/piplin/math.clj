(ns piplin.math
  (:use (piplin types)))

(derive-type Long :j-long)
(defmethod dopromote 
  :j-long
  [type obj]
  (if (isa-type? obj :j-long)
    obj
    (error "Cannot promote" obj "to Long")))

(defmulti constrain
  "Takes a type and a value and constrains the value to
  the type's range."
  (fn [type val] (kindof type))
  :hierarchy types)

(defmethod constrain
  :default
  [a b] b)

(defmulti check
  "Takes an instance and verifies that it meets the
  constraints of its type"
  kindof
  :hierarchy types)

(defmethod check
  :default
  [a] a)

(defrecord Instance [type val])
(defn instance
  "Creates an instance of the type with value val"
  [type val & more]
  (let-safe [val (if (some #{:constrain} more)
                   (constrain type val)
                   val)]
            (check
              (merge (Instance. type val)
                     {:type type :kind (:kind type)}))))

(defrecord UIntM [n])
(defn uintm [n]
  "Make a new uintm type object"
  (merge (UIntM. n)
         {:kind :uintm}))

(defmethod constrain
  :uintm
  [this init-val]
  "Takes a uintm (this) and the value
  to initialize the new instance with,
  and constrains the number to be in the
  range of uintm"
  (mod init-val (bit-shift-left 1 (:n this))))

(defmethod check
  :uintm
  [inst]
  (let [n (get-in inst [:type :n])
        v (:val inst)
        maxval (dec (bit-shift-left 1 n))]
    (if (< v 0)
      (error "uintm must be positive:" v)
      (if (> v maxval)
        (error "uintm" n "must be less than" maxval
               ", got:" v)
        inst))))

(defmethod dopromote
  :uintm
  [this obj]
  (cond
    (= (:type obj) this) obj ;Already correct
    (= (:kind obj)
       (:kind this)) (error
                       "Incompatible type instances: " this
                       "and" (type obj))
    (isa-type? (kindof obj) :j-long) (instance this obj)
    :else (error "Don't know how to promote to :uintm from"
                 (type obj))))


(defn nary-dispatch
  "Dispatching logic used by binary math operations"
  ([] ::nullary)
  ([x] (kindof x))
  ([x y] [(kindof x) (kindof y)])
  ([x y & more] ::n-ary))

(defmacro defbinop
  "Defines a generic function for a binary operation
  on numeric types. Uses the function in clojure/core
  to provide implementations for Numbers. Handles
  nullary and n-ary invocations by returning zero and
  the left-associative folds, respectively."
  [op zero]
  (let [core-op (gensym (str (name op) "core"))]
    `(do
       (defmulti ~op nary-dispatch :hierarchy types)
       (defmethod ~op ::nullary [] ~zero)
       (defmethod ~op ::n-ary
         [~'x ~'y & ~'more]
         (if ~'more
           (recur (~op ~'x ~'y) (first ~'more) (next ~'more))
           (~op ~'x ~'y)))
       (def ~core-op (ns-resolve 'clojure.core '~op))
       (defmethod ~op [:number :number]
         [~'x ~'y]
         (~core-op ~'x ~'y))
       (defmethod ~op [:j-long :j-long]
         [~'x ~'y]
         (~core-op ~'x ~'y)))))

(defbinop + 0)
(defbinop - 0)
(defbinop * 0)
(defbinop bit-and 0)
(defbinop bit-or 0)
(defbinop bit-xor 0)

(defmacro defbinopimpl
  "Defines implementation of a binop on a piplin kind.
  Requires the kind to attempt to unify to, the list
  of types which can be promoted to the kind, and an
  fntail that takes 2 arguments and returns the result.
  The implementation will return an error if the
  unification failed."
  [op k bases & fntail]
  (let [impl-name (gensym (str (name op) (name k)))
        impl-body `(defn ~impl-name
                     [x# y#]
                     (let-safe [[x# y#] (type-unify ~k x# y#)]
                       ((fn ~@fntail) x# y#)))
        k-bases (map #(vector k %) bases)
        dispatches (concat k-bases
                           (map reverse k-bases)
                           [[k k]])
        bodies (map (fn [[a b]]
                      `(defmethod ~op [~a ~b]
                         [~'x ~'y]
                         (~impl-name ~'x ~'y)))
                    dispatches)]
    `(do
       ~impl-body
       ~@bodies)))

(defbinopimpl + :uintm [:j-long]
  [x y]
  (instance (:type x) (+ (:val x) (:val y)) :constrain))

(defbinopimpl - :uintm [:j-long]
  [x y]
  (instance (:type x) (- (:val x) (:val y)) :constrain))

(defbinopimpl * :uintm [:j-long]
  [x y]
  (instance (:type x) (bit-and (* (:val x) (:val y) )) :constrain))

(defbinopimpl bit-and :uintm [:j-long]
  [x y]
  (instance (:type x) (bit-and (:val x) (:val y)) :constrain))

(defbinopimpl bit-or :uintm [:j-long]
  [x y]
  (instance (:type x) (bit-or (:val x) (:val y)) :constrain))

(defbinopimpl bit-xor :uintm [:j-long]
  [x y]
  (instance (:type x) (bit-xor (:val x) (:val y)) :constrain))

;successfully blocked
;(+ ((uintm 3) 3) ((uintm 4) 3))

;(+ 42 3)
;(+ ((uintm 8) 3) ((uintm 8) 4))

;(+ ((uintm 8) 3) -4)
;(+ 3 ((uintm 8) 4))

(comment
  TODO
  - error reporting (file/lineno)
; - enforce boundaries of uintm [0 ..< (2 ^ n)]
  - implement other operations on uintm
  - implement sintm, sints, uints, etc. (e type is hard)
  - design AST
  - implement sim function, ast, and
    verilog module binding macro(s)
  - implement simulation with syn/ack)
