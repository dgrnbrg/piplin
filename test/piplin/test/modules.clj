(ns piplin.test.modules
  (:use clojure.test)
  (:refer-clojure :as clj :exclude [not= + - * inc dec < > <= >= = cast not bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:use [piplin.types boolean numbers core-impl binops uintm])
  (:use plumbing.core)
  (:use [piplin types math sim modules]))

(deftest module-counter'
  (let [m (compile-root (modulize :root {:x (fnk [x] (inc x))}
                                  {:x ((uintm 8) 0)}))]
    (are [x] (= x (get (last (sim m x)) [:root :x]))
         0 5 10)))

;This test should see a counter that grows arithmatically
(deftest submodule-test
  (let [m (modulize
            {:x (fnk [step x] (+ x step))}
            {:x ((uintm 8) 0)})
        root (modulize :root
               {:step (fnk [step] (inc step))
                :output (fnk [step]
                             (:x (m :step step)))}
               {:step ((uintm 8) 0)})
        system (compile-root root)
        do-sim (fn [cycles]
                 (get (last (sim system cycles)) [:root :output]))]
    (is (= (do-sim 0) ((uintm 8) 0)))
    (is (= (do-sim 1) ((uintm 8) 0)))
    (is (= (do-sim 2) ((uintm 8) 1)))
    (is (= (do-sim 3) ((uintm 8) 3)))
    (is (= (do-sim 4) ((uintm 8) 6)))
    (is (= (do-sim 5) ((uintm 8) 10)))
    (is (= (do-sim 6) ((uintm 8) 15)))
    (is (= (do-sim 7) ((uintm 8) 21)))
    (is (= (do-sim 8) ((uintm 8) 28)))
    (is (= (do-sim 9) ((uintm 8) 36)))))

(def counter
  (modulize {:x (fnk [x] (inc x))} {:x ((uintm 8) 0)}))

(def delayer
  (modulize {:out (fnk [in] in)} {:out ((uintm 8) 0)}))

(def delayer-holder
  (modulize :root {:out (fnk []
                             (let [counter (counter)
                                   delayer (delayer :in (:x counter))]
                               (:out delayer)))}
            nil))

(deftest delayer-test
  (let [m (compile-root delayer-holder)]
    (are [cycle value] (= (get (last (sim m cycle)) [:root :out]) value)
         0 0
         1 0
         2 1
         3 2
         4 3
         5 4
         6 5)))

(deftest sim-fail-test
  (is (thrown? java.lang.AssertionError (sim {} 10)))
  (is (thrown? java.lang.AssertionError
               (sim (compile-root
                      (modulize
                        {:a (fnk [x] x)} {})
                      :x (input "foo" (uintm 8)))
                    10))))
