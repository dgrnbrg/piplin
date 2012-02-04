(ns piplin.sim)

(comment
  A fn can be added to the schedule in K cycles or
  when some state elt changes.

  Sorted set of things that happen in K cycles to support
  efficient addition of new events.

  Map to store state elts. When elts change, updates
; are scheduled in the next cycle (enhancement: same cycle?).

  A fn gets the state that it's dependent on as its input
  and must return the new value of its state and a map from
  events to the fns to run when they occur. This allows for
  maximum flexibility to have purely combinational logic or
  to have callbacks that send messages to other processes
  and block on their response.
  )

(comment
; init:
  - cycle = 0
  - next-state = apply initial-fns to initial-states
  - generate events by (diff initial-states next-state)
  - get fns by using those events + next cycle event on
    the just-registered events
; cycle:
; - if cycle == last-cycle: exit
  - next-state, new-events = apply fns state
  - events = diff state next-state
  - fns, old-events = schedule [cycle events] [new-events old-events]
  - state = next-state
  - cycle = cycle + 1
  )

(defn- resolve-args
  "Takes a map of state and a vector of keys and returns
  a seq of the values from the map."
  [cycle state keys]
  (let [state (assoc state :cycle cycle)]
    (map #(get state %) keys)))

(defn run-cycle
  "Takes a map from names to values of state elts
  and a map from fns to vectors of the names of the
  state elts that are their arguments.

  Applies the argument to each fn, which must return
  a map from the names of the state elts it wishes to
  update to their new values. All the returned maps'
  keysets must be disjoint. Each fn also returns a map
  from cycle numbers and state names to fns to be run
  when they're reached/changed, respectively."
  [cycle state fns]
  (let [fn-results (map #(apply (key %)
                                (resolve-args
                                  cycle
                                  state
                                  (val %)))
                        fns)
        delta-state (apply merge-with
                           (fn [x y]
                             (throw (AssertionError.
                                      "nondisjoint")))
                           (map first fn-results))
        new-reactors (apply merge-with
                            concat
                            (map second fn-results))]
    [delta-state new-reactors]))

(defn what-changed
  "Takes a complete state map and a delta state map to
  apply to it. Computes the new state and returns it
  along with a list of keys of which state elts changed."
  [state delta-state]
  (let [new-state (merge state delta-state)
        changed-elts (->> delta-state
                       (remove #(= (val %)
                                      ((key %) state)))
                       (apply concat)
                       (apply hash-map))]
    [new-state changed-elts]))

(defn next-fns
  "Takes a new cycle and list of events and a map of
  reactors and returns the functions to be run next cycle
  and the map of untriggered reactors"
  [cycle events reactors]
  (let [next-cycle-fns (get reactors cycle)
        event-fns (apply merge (map #(get reactors %) events))
        next-fns (merge event-fns next-cycle-fns)
        unused (apply dissoc reactors cycle events)]
    [next-fns unused]))

(defn exec-sim
  "
  initial-state is a map from names to values of state elts.
  
  initial-fns is a map from fns to a vector of the names of
  the state-elts they require (they will be applied to the
  fn in the order they're declared).

  max-cycle is the cycle to run until."

  ;entry point
  ([initial-state initial-fns max-cycle]
   (exec-sim 0 initial-fns initial-state {}))

  ([cycle fns state reactors max-cycle]
   (let [[delta-state new-reactors] (run-cycle cycle state fns)
         [state events] (what-changed state delta-state)
         reactors (merge-with concat reactors new-reactors)
         next-cycle (inc cycle)
         [fns reactors] (next-fns next-cycle events reactors)]
     (if (= next-cycle max-cycle)
       state
       (recur next-cycle fns state reactors max-cycle)))))
