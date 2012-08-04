(ns piplin.test.arrays
  (:refer-clojure :exclude [cast])
  (:use [clojure.test])
  (:require [piplin.core :as p])
  (:use [piplin.types [array]]))

(deftest array-basics
  (let [a (array (p/uintm 8) 4)
        inst (p/cast a [8 7 0 3])
        [x y z w] inst
        roundtrip (->> inst
                    (p/serialize)
                    (p/deserialize a)) ]
    (is (piplin.types/pipinst? inst))
    (is ((p/uintm 8) 8) x)  
    (is ((p/uintm 8) 7) y)  
    (is ((p/uintm 8) 0) z)  
    (is ((p/uintm 8) 3) w)
    (is (= 4 (count inst)))
    (is (piplin.types/pipinst? roundtrip))
    (is (p/= inst roundtrip))))

(deftest array-uninst
  (let [a (array (p/anontype :boolean) 7)
        inst (piplin.types/uninst (a (repeat 7 false)))]
    (is (not= inst (a (repeat 7 false))))  
    (is (= ((piplin.modules/make-sim-fn inst)) (a (repeat 7 false))))))

(p/defmodule filler []
  [:outputs [a (p/cast (array (p/anontype :boolean) 8)
                     (repeat 8 false))]
   :feedback [x ((p/uintm 3) 0)]]
  (p/connect x (p/inc x))
  (p/condp p/= x
    1 (p/connect a (assoc a 0 true))  
    2 (p/connect a (assoc a 1 true))  
    3 (p/connect a (assoc a 2 true))  
    4 (p/connect a (assoc a 3 true))  
    5 (p/connect a (assoc a 4 true))  
    6 (p/connect a (assoc a 5 true))  
    7 (p/connect a (assoc a 6 true))
    (p/connect a a)))

(deftest array-module
  (let [[state fns] (p/make-sim (filler))]
    (is (p/= ((p/exec-sim state fns 1) [:a])
             [true false false false false false false false]))
    (is (p/= ((p/exec-sim state fns 3) [:a])
             [true true true false false false false false]))  
    (is (p/= ((p/exec-sim state fns 5) [:a])
             [true true true true true false false false]))
    (is (p/= ((p/exec-sim state fns 10) [:a])
             [true true true true true true true false]))))
