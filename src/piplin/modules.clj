(ns piplin.modules)

(comment
  The syntax for a module is

  (module :inputs
          [a type
           b type]
          :outputs
          [o1 init-val]
          :feedback
          [x init-val]
          :modules
          [sub1 (instantiate)
           sub2 (instantiate) :only [:port1 :port2]]
    body...)

  The body should include connections for everything that
  needs one.

  There are 4 sections, provided in this order, each of
  which should have a nonempty list of symbol/value pairs,
  like "let". The value varies from being a type to an
  initialization value to a module instance.

  Module instances can use :exclude and :only like for imports
  to only refer to certain ports and leave the rest unconncted.

  The body can contain connections between registers. It's the
  responsibility of a semantic validation/error checking pass
  over the tree to verify that inputs and outputs are fully
  connected.

  The :inputs section is for inputs to the process. The
  :outputs section is for registered outputs from the
  process. The :feedback section is for registers that
  aren't outputs (kind of like a private :output). :modules
  is for other submodules, and then all the specified ports
  (no spec = all ports) must be connected somewhere in
  the body.

  The above module, of course, evaluates to the following

  {:type :module
   :inputs {:a type :b type}
   :outputs {:o1 init-val}
   :feedback {:x init-val}
   :modules {:sub1 {:inst (instantiate)}
             :sub2 {:inst (instantiate) :only [:port1 :port2]}}
   :body [body...]}
  )

(defn explode [& msgs]
  (throw (RuntimeException. (apply str msgs))))

(defmacro module [config & body]
  (if-not (even? (count config))
    (explode "Odd number of elements in module args."))
  (let [params (partition 2 config)]
    (loop [unparsed (vec params) inputs nil outputs nil feedback nil modules nil]
      (if (seq unparsed)
        (let [[section decls] (first unparsed)]
          (if-not (vector? decls)
            (explode "Decls must be a vector")
            (let [keys (map keyword (take-nth 2 decls))
                  vals (take-nth 2 (rest decls))
                  decls (zipmap keys vals)]
              ;check here that decls are in the correct form
              (condp = section
                :inputs (if inputs
                          (explode "multiple definition of inputs" inputs)
                          (recur (rest unparsed) decls outputs feedback modules))
                :outputs (if outputs
                          (explode "multiple definition of outputs")
                          (recur (rest unparsed) inputs decls feedback modules))
                :feedback (if feedback
                          (explode "multiple definition of feedback")
                          (recur (rest unparsed) inputs outputs decls modules))
                :modules (if modules
                          (explode "multiple definition of modules")
                          (recur (rest unparsed) inputs outputs feedback decls))
                (explode "unknown section header:" section)))))
        `{:type :module
          :inputs ~inputs
          :outputs ~outputs
          :feedback ~feedback
          :modules ~modules
          :body [~@body]}))))
