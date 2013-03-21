(ns piplin.test.johnson
  (:use piplin.test.util)
  (:use clojure.test
        plumbing.core)
  (:require [piplin.core :as p]))

(defn johnson [w]
  (p/modulize :root
    {:q (fnk [q]
             (p/bit-cat (p/bit-slice q 0 (dec w))
                        (p/bit-not (p/bit-slice q (dec w) w))))}
    {:q (p/cast (p/bits w) 0)}))

(deftest basic-johnson-test
  (let [m (p/compile-root (johnson 4))]
    (are [cycles bits] (= (get (last (p/sim m cycles)) [:root :q]) bits)
         0 #b0000
         1 #b0001
         2 #b0011
         3 #b0111
         4 #b1111
         5 #b1110
         6 #b1100
         7 #b1000
         8 #b0000)))

(deftest basic-johnson-verilog-test
  (icarus-test (p/verify
                 (johnson 8) 500)))
