(ns piplin.sim)

;TODO: still needs max cycle to run until

(defn sim-elt [logic argnames fwd]
  "Returns a new simulation element with a function logic "
  "and inputs argnames and value-forwarder fwd"
  {:dests []
   :logic logic
   :fwd-fn fwd
   :inputs (zipmap argnames (repeat []))})

(defn add-dest [se name dest]
  "Adds [name dest] to the list of :dests in se"
  (let [dests (:dests se)]
    (assoc se :dests (conj dests [name dest]))))

(defn add-input [sim-elt key val]
  "Appends an input named key with value val to the sim-elt"
  (let [input-key [:inputs key]
        input-vec (get-in sim-elt input-key)]
    (assoc-in sim-elt input-key (conj input-vec val))))

(defn- map-vals [f m]
  "takes a function of one argument and a map and returns a new map "
  "with the same keys and whose values are the (f old-value)"
  (reduce (fn [tmp-map old-entry]
            (assoc tmp-map (key old-entry) (f (val old-entry))))
          {}
          m))

(defn send-from [sim-elt val]
  "Takes a sim-elt and a value, and sends the val to all of the sim-elt's "
  "destinations. If a sim-elt is a pipeline stage, can be used to "
  "initialize the stage"
  (doseq [addr (:dests sim-elt)]
    (apply (:fwd-fn sim-elt) val addr)))

(defn process-sim-elt [sim-elt]
  "takes a sim-elt and, if every input list has at least one element, "
  "removes the head of each element, computes the result, and invokes "
  "(fwd result name dest) for every destination. Returns the procesed "
  "sim-elt."
  (if (every? #(seq (val %)) (:inputs sim-elt)) ;all inputs are ready
    (let [inputs (:inputs sim-elt)
          inputs-first (map-vals first inputs)
          inputs-rest (map-vals rest inputs)
          result ((:logic sim-elt) inputs-first)]
      (send-from sim-elt result)
      (recur (assoc sim-elt :inputs inputs-rest)))
    sim-elt))

(defn add-input-process [sim-elt key val]
  "Adds the value to the input named key for the sim-elt"
  (process-sim-elt (add-input sim-elt key val)))

(defn- agent-fwd [result name dest]
  "fwd function for agents"
  (send dest add-input-process name result))

(comment

(def adder (sim-elt (fn [{x :x y :y}]
                      (+ x y))
                    [:x :y]
                    nil))

(defn mk-logger []
                 (let [a (atom [])
                       se (assoc (sim-elt #(swap! a conj (:a %)) [:a] nil)
                                 :log-atom a)]
                   se))

(def logger (agent (mk-logger)))

(def printer (sim-elt (fn [{a :a}]
                        (println "printing result:" a "end result"))
                      [:a]
                      nil))

(def p (agent (assoc printer :fwd-fn agent-fwd)))

(let [agent-adder (assoc adder :fwd-fn agent-fwd)]
  (def a1 (agent agent-adder))
  (def a2 (agent agent-adder)))

(send a1 add-dest :x a2)
(send a2 add-dest :a p)

(send a1 add-input-process :y 2)
(send a2 add-input-process :y 7)

(send a1 add-input-process :x 1)

(def counter (agent (assoc adder :fwd-fn agent-fwd)))

(defn mask [x] nil)

(send counter add-dest :a logger)

(mask (send counter add-dest :x counter))

(await counter)

(send-from @counter 0)

(await counter)

(pprint (map first (:dests @counter)))

(dotimes [n 20] (send counter add-input-process :y 1))

(use 'clojure.pprint)

(await counter)

(pprint (dissoc @counter :dests))

(pprint (dissoc @logger :dests))

(send p add-input-process :a "hello world")

  )
