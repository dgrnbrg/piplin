(ns piplin.test.arrays
  (:refer-clojure :exclude [cast])
  (:use [clojure.test])
  (:use piplin.test.util
        plumbing.core)
  (:require [piplin.core :as p])
  (:use [piplin.types [array]]))

(deftest array-basics
  (let [a (array (p/uintm 8) 4)
        inst (p/cast a [8 7 0 3])
        [x y z w] inst
        roundtrip (->> inst
                       (p/serialize)
                       (p/deserialize a)) ]
    (is (piplin.protocols/pipinst? inst))
    (is ((p/uintm 8) 8) x)
    (is ((p/uintm 8) 7) y)
    (is ((p/uintm 8) 0) z)
    (is ((p/uintm 8) 3) w)
    (is (= 4 (count inst)))
    (is (piplin.protocols/pipinst? roundtrip))
    (is (p/= inst roundtrip))))

(deftest array-uninst
  (let [a (array (p/anontype :boolean) 7)
        inst (piplin.types/uninst (p/cast a (repeat 7 false)))]
    (is (not= inst (p/cast a (repeat 7 false))))
    (is (= ((piplin.modules/make-sim-fn inst)) (p/cast a (repeat 7 false))))))

(def filler-fns
  {:a (fnk [mem x]
           (p/store mem
                    (p/not= x 0)
                    (p/cast (p/uintm 3)
                            (p/condp p/= x
                              1 0
                              2 1
                              3 2
                              4 3
                              5 4
                              6 5
                              7 6
                              0))
                    true))
   :mem0 (fnk [mem] (get mem 0))
   :mem1 (fnk [mem] (get mem 1))
   :mem2 (fnk [mem] (get mem 2))
   :mem3 (fnk [mem] (get mem 3))
   :mem4 (fnk [mem] (get mem 4))
   :mem5 (fnk [mem] (get mem 5))
   :mem6 (fnk [mem] (get mem 6))
   :mem7 (fnk [mem] (get mem 7))
   :x (fnk [x] (p/inc x))})

(def filler-regs
  {:x ((p/uintm 3) 0)
   :mem (p/cast (array
                  (p/anontype :boolean)
                  8)
                (repeat 8 false))})

(def filler
  (p/modulize :root
              filler-fns
              filler-regs))

(deftest array-module
  (are [cycle state]
       (p/= (get (last (p/sim (p/compile-root filler)
                              cycle))
                 [:root :mem])
            (p/cast (array (p/anontype :boolean) 8)
                    state))
       2 [true false false false false false false false]
       4 [true true true false false false false false]
       6 [true true true true true false false false]
       10 [true true true true true true true false])
  (icarus-test (p/verify filler 50)))

(def double-filler
  (p/modulize :root
              (assoc filler-fns
                     :b
                     (fnk [mem x]
                          (p/store mem
                                   true
                                   x
                                   false)))
              filler-regs))

(deftest array-module-multi-write
  (are [cycle state]
       (p/= (get (last (p/sim (p/compile-root double-filler)
                              cycle))
                 [:root :mem])
            (p/cast (array (p/anontype :boolean) 8)
                    state))
       2 [true false false false false false false false]
       3 [true true false false false false false false]
       4 [true true true false false false false false]
       5 [true true true true false false false false]
       6 [true true true true true false false false]
       7 [true true true true true true false false]
       8 [true true true true true true true false]
       9 [false true true true true true true false]
       10 [true false true true true true true false]
       11 [true true false true true true true false])
  (icarus-test (p/verify double-filler 50)))

(def states (p/enum #{:foo :bar :baz :quux}))

(def replay-data (p/cast (array states 30) (take 30 (cycle [:foo :bar :foo :baz :foo :quux]))))

(defn ifelse-memory
  "Returns a synthesizable function which takes an
   address and returns the value associated with that
   key. Requires an `:else` key in the map for any unknown
   slots. This key cannot be left out."
  ([data]
   (ifelse-memory (dissoc data :else) (:else data)))
  ([data elseval]
   (if (seq data)
     (let [[[k v] & more] (seq data)]
       (fn [addr]
         (p/mux2 (p/= k addr)
                 v
                 ((ifelse-memory more elseval) addr))))
     (fn [addr] elseval))))

(def replay-rom
  (ifelse-memory (zipmap (range) replay-data) (p/cast states :foo)))

(def replayer
  (let [maybe-type (p/maybe states)]
    (p/modulize :root
                {:index (fnk [index] (p/inc index))
                 :o (fnk [index] (p/cast maybe-type {:just (replay-rom index)}))}
                {:index ((p/uintm (p/log2 (count replay-data))) 0)
                 :o (p/cast maybe-type
                            {:nothing nil})})))

(deftest replay-test
  (let [m (p/compile-root replayer)
        run-sim #(get (last (p/sim m %)) [:root :o])
        ->type #(p/cast (p/maybe states) {:just %})]
    (are [cycle value] (p/= (run-sim cycle) (->type value))
         1 :foo
         2 :bar
         4 :baz
         17 :foo)))

(def sequencer
  (p/modulize :root
              {:index (fnk [index] (p/inc index))
               :x (fnk [index]
                       (replay-rom index))
               :rfile-index (fnk [x]
                                 (p/cast (p/uintm 2)
                                         (p/condp p/= x
                                           :bar 0
                                           :baz 1
                                           ;TODO: support dangling else
                                           ;:quux
                                           2)))
               :in (fnk [rfile rfile-index]
                        (get rfile rfile-index))
               :rfile
               (fnk [rfile-index in x rfile]
                    (p/store
                      rfile
                      (p/not= x :foo)
                      rfile-index
                      (p/inc in)))}
              {:index ((p/uintm (p/log2 (count replay-data))) 0)
               :rfile (p/cast (array (p/uintm 4) 3) [0 0 0])
               :in ((p/uintm 4) 0)}))

(deftest sequencer-test
  (let [m (p/compile-root sequencer)
        run-sim #(get (last (p/sim m %)) [:root :rfile])]
    (are [cycle value] (p/= (run-sim cycle) value)
         1 [0 0 0]
         2 [1 0 0]
         4 [1 1 0]
         6 [1 1 1]
         36 [6 6 6]))
  (icarus-test (p/verify
                 sequencer 50)))

(def memory-file
  (p/modulize :root
              {:do-write (fnk [mem i]
                              (piplin.types.array/store
                                mem
                                true
                                i
                                (p/inc (get mem i))))
               :mem0 (fnk [mem] (get mem 0))
               :mem1 (fnk [mem] (get mem 1))
               :mem2 (fnk [mem] (get mem 2))
               :mem3 (fnk [mem] (get mem 3))
               :i (fnk [i] (p/inc i))}
              {:i ((p/uintm 2) 0)
               :mem (p/cast (array (p/uintm 4) 4)
                            [0 0 0 0])}))

(deftest memory-test
  (let [m (p/compile-root memory-file)]
    (are [cycle state]
         (p/= (get (last (p/sim m cycle)) [:root :mem])
              state)
         1 [1 0 0 0]
         3 [1 1 1 0]
         8 [2 2 2 2]
         34 [9 9 8 8])))

(deftest memory-verilog-test
  (icarus-test (p/verify
                 memory-file 50)))

(def cycling
  (let [a-type (array (p/uintm 3) 2)]
    (p/modulize :root
                {:pod (fnk [pod]
                           (let [[x y] pod]
                             (p/cast a-type [(p/inc x) (p/inc y)])))}
                {:pod (p/cast a-type [0 2])})))

(deftest cycling-test
  (let [m (p/compile-root cycling)
        a #(p/cast (array (p/uintm 3) 2) %)]
    (are [cycle value] (p/= (a value)
                            (get (last (p/sim m cycle))
                                 [:root :pod]))
         0 [0 2]
         1 [1 3]
         2 [2 4]
         3 [3 5]
         4 [4 6]
         6 [6 0])))
