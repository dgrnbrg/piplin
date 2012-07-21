(ns piplin.test.sim
  (:use [piplin sim modules math types])
  (:use [piplin types modules sim mux connect])
  (:use [piplin.types numbers core-impl binops uintm])
  (:refer-clojure :as clj :exclude [not= cond + - * inc dec bit-or bit-xor bit-and bit-not condp < > <= >= = cast])
  (:use clojure.test))

(deftest what-changed-test
  (is (= (what-changed {:a 1 :b 2 :c 3 :d 4}
                       {:b 2 :c 3 :d 5})
         [{:a 1, :c 3, :b 2, :d 5}
          {:d 5}])
      "check that we identify everything that
      changed and merge properly"))

(deftest next-fns-test
  (is (= (next-fns 3 [:a :b :c]
                   {3 {:f1 []}
                    4 {:f2 []}
                    :b {:f3 [] :f5 []}
                    :d {:f4 [] :f6 []}})
         [{:f1 [] :f3 [] :f5 []}
          {4 {:f2 []} :d {:f4 [] :f6 []}}])
      "next functions and remaining functions
      correctly identified"))

(deftest run-cycle-test
  (let [[delta reactors] (run-cycle
                           2
                           {:a 1 :b 2}
                           {(fn [a b cycle]
                              [{:c (+ a b)}
                               {(inc cycle) [#(str "next")]
                                :b [#(str "baz")]}])
                            [:a :b :cycle]
                            (fn [a]
                              [{:a 22}
                               {:b [#(str "bar")]}])
                            [:a]})]
    (is (= delta {:c 3 :a 22})
        "make sure full delta is generated")
    (is (= ((first (get reactors 3))) "next")
        "get reactor for cycle event")
    (is (= (set (map #(%) (:b reactors)))
           (set ["bar" "baz"]))
        "combining reactors for same event"))
  (is (thrown? AssertionError
               (run-cycle
                 2
                 {:a 1 :b 2}
                 {(fn [a b cycle]
                    [{:c (+ a b)}
                     {(inc cycle) [#(str "next")]
                      :b [#(str "baz")]}])
                  [:a :b :cycle]
                  (fn [a]
                    [{:a 22 :c -1}
                     {:b [#(str "bar")]}])
                  [:a]}))))

(deftest exec-sim-test
  (let [[counterfn arglist] (every-cycle (fn [x] (inc x))
                                         [:count]
                                         :count)]
    (is (= (exec-sim {:count 0} {counterfn arglist} 10)
           {:count 10})))
  (let [mod (module [:outputs [c (instance (uintm 8) 0)]]
                    (connect c (+ c 1)))
        sim (make-sim mod)
        init-state (first sim)
        init-fns (second sim)]
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [:c])
           (instance (uintm 8) 10))
        "ran and counted up to 10")
    (is (= (get (exec-sim init-state
                          init-fns
                          257)
                [:c])
           (instance (uintm 8) 1))
        "ran and counted up to 257/1 (numeric type must work)"))
  (let [mod (module [:outputs [c (instance (uintm 4) 0)]
                     :modules [sub (module [:outputs [x (instance (uintm 4) 1)]]
                                           (connect x (+ x 2)))]]
                    (connect c (+ c 1)))
        sim (make-sim mod)
        state (first sim)
        fns (second sim)
        u4 #(instance (uintm 4) %)]
    (is (= (select-keys (exec-sim state fns 7)
                        [[:c]
                         [:sub :x]])
           {[:c] (u4 7) [:sub :x] (u4 15)}))))
           

