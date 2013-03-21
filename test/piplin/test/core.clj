(ns piplin.test.core
  (:require [piplin.core :as p])
  (:use [clojure.test]
        plumbing.core))

(defn counter [b]
  (p/modulize :root
    {:x (fnk [x] (p/inc x))}
    {:x ((p/uintm b) 0)}))

(deftest counter-test
  (let [result (last (p/sim (p/compile-root (counter 8)) 10))]
    (is (= (get result [:root :x]) ((p/uintm 8) 10)))))
