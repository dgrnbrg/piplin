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


(def filler
  (p/modulize :root
    {:a (fnk [a x]
             (p/store a (p/not= x 0)
                      (p/condp p/= x
                               1 0
                               2 1
                               3 2
                               4 3
                               5 4
                               6 5
                               7 6
                               0)
                      true))
     :x (fnk [x] (p/inc x))}
    {:a (p/cast (array (p/anontype :boolean) 8)
                (repeat 8 false))
     :x ((p/uintm 3) 0)}))

(deftest array-module
  (are [cycle state]
       (p/= (get (last (p/sim (p/compile-root filler)
                              cycle))
                 [:root :a])
            (p/cast (array (p/anontype :boolean) 8)
                    state))
       1 [true false false false false false false false]
       3 [true true true false false false false false]
       5 [true true true true true false false false]
       10 [true true true true true true true false]))

(comment
(def states (p/enum #{:foo :bar :baz :quux}))

(def replay-data (p/cast (array states 30) (take 30 (cycle [:foo :bar :foo :baz :foo :quux]))))

(p/defmodule replayer [data]
  [:feedback [tape data
              index ((p/uintm (p/log2 (count data))) 0)]
   :outputs [o (p/cast (p/maybe (piplin.protocols/typeof (nth data 0))) {:nothing nil})]]
  (p/connect index (p/inc index))
  (p/connect o {:just (get data index)}))

(deftest replay-test
  (let [m (replayer replay-data)
        [state fns] (p/make-sim m)
        ->type #(p/cast (p/maybe states) {:just %})]
    (is (p/= (get (p/exec-sim state fns 1) [:o]) (->type :foo)))
    (is (p/= (get (p/exec-sim state fns 2) [:o]) (->type :bar)))
    (is (p/= (get (p/exec-sim state fns 4) [:o]) (->type :baz)))
    (is (p/= (get (p/exec-sim state fns 17) [:o]) (->type :foo)))))

(p/defmodule sequencer [data]
  [:feedback [tape data
              index ((p/uintm (p/log2 (count data))) 0)]
   :outputs [rfile (p/cast (array (p/uintm 4) 3) [0 0 0])]]
  (p/connect index (p/inc index))
  (p/connect rfile
             (p/condp p/= (get tape index)
               :bar (update-in rfile [0] p/inc)
               :baz (update-in rfile [1] p/inc)
               :quux (update-in rfile [2] p/inc)
               rfile)))

(deftest sequencer-test
  (let [m (sequencer replay-data)
        [state fns] (p/make-sim m)]
    (is (p/= (get (p/exec-sim state fns 1) [:rfile]) [0 0 0]))
    (is (p/= (get (p/exec-sim state fns 2) [:rfile]) [1 0 0]))
    (is (p/= (get (p/exec-sim state fns 4) [:rfile]) [1 1 0]))
    (is (p/= (get (p/exec-sim state fns 6) [:rfile]) [1 1 1]))
    (is (p/= (get (p/exec-sim state fns 36) [:rfile]) [6 6 6]))))

(p/defmodule memory-file []
  [:feedback [mem (p/cast (array (p/uintm 4) 4) [0 0 0 0])
              i ((p/uintm 2) 0)]]
  (p/connect i (p/inc i))
  (p/connect (get mem i) (p/inc (get mem i))))

(deftest memory-test
  (let [m (memory-file)
        [state fns] (p/make-sim m)]
    (is (p/= (get (p/exec-sim state fns 1) [:mem]) [1 0 0 0]))
    (is (p/= (get (p/exec-sim state fns 3) [:mem]) [1 1 1 0]))
    (is (p/= (get (p/exec-sim state fns 8) [:mem]) [2 2 2 2]))
    (is (p/= (get (p/exec-sim state fns 34) [:mem]) [9 9 8 8]))))

(deftest memory-verilog-test
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 1))
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 2))
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 3))
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 4))
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 10))
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 30))
  (icarus-test (p/modules->verilog+testbench
                 (memory-file) 50)))

(p/defmodule cycling []
  [:feedback [pod (p/cast (array (p/uintm 3) 2) [0 2])]]
  (let [[x y] pod]
    (p/connect pod [(p/inc x) (p/inc y)])))

(deftest cycling-test
  (let [m (cycling)
        a #(p/cast (array (p/uintm 3) 2) %)
        [state fns] (p/make-sim m)]
    (is (p/= (a [0 2])
             (get (p/exec-sim state fns 0) [:pod])))
    (is (p/= (a [1 3])
             (get (p/exec-sim state fns 1) [:pod])))
    (is (p/= (a [2 4])
             (get (p/exec-sim state fns 2) [:pod])))
    (is (p/= (a [3 5])
             (get (p/exec-sim state fns 3) [:pod])))
    (is (p/= (a [4 6])
             (get (p/exec-sim state fns 4) [:pod])))
    (is (p/= (a [6 0])
             (get (p/exec-sim state fns 6) [:pod]))))))
