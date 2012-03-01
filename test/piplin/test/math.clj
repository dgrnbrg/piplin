(ns piplin.test.math
  (:use clojure.test)
  (:import slingshot.ExceptionInfo)
  (:use [slingshot.slingshot :only [throw+]])
  (:use [piplin types math modules sim]))

(deftest j-long-test
  (is (= 3 (+ 1 2)))
  (is (= 6 (* 2 3)))
  (is (thrown? ExceptionInfo (promote :j-long "lx"))))

(deftest float-test
  (is (= 3.5 (+ 1.5 2))))

(deftest =test
  (is (= 1 1 1))
  (is (not (= 1 1 2)))
  (is (= "hello" "hello"))
  (is (= [1 \a] [1 \a]))
  (is (< 3 7))
  (is (> 0 -1.2))
  (is (>= -3.3 -3.3))
  (is (not (<= 4 1)))
  (is (not= 1 2))
  (is (not (not= 0 0))))

(deftest uintm-test
  (letfn [(um8 [v] (instance (uintm 8) v))]
    (is (= (+ (um8 200) 1) (um8 201)))
    (is (= (+ (um8 200) 60) (um8 4)))
    (is (= (+ 1 2 3 (um8 4)) (um8 10)))
    (is (= (instance (uintm 8) -1 :constrain) (um8 255)))
    (is (= (instance (uintm 8) 256 :constrain) (um8 0)))
    (is (thrown? ExceptionInfo (promote (uintm 3) "lx")))
    (is (thrown? ExceptionInfo (promote (uintm 3) -1)))
    (is (thrown? ExceptionInfo (promote (uintm 3) 100)))
    (is (= (- 0 (um8 1)) (um8 255)))))

(deftest binop-error-test
  (let [e (error "hi")]
    (is (= (try-errors (+ 0 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 2 3 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 (throw+ e) 3 (throw+ e))) e))
    (is (= (try-errors (+ (throw+ e) 0)) e))
    (is (= (try-errors (+ (throw+ e) (throw+ e))) e))
    (is (= (try-errors (bit-cat (throw+ e) (throw+ e))) e))))

(deftest bits-test
  (let [b8 #(promote (bits 8) %)]
    (is (= (value (b8 0xa0)) [1 0 1 0 0 0 0 0]))
    (is (= (value (bit-cat (b8 0xa0) (bit-cat (b8 0xfe))))
           [1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 0]))
    (is (= (value (bit-slice (bit-cat (b8 0xf) (b8 0xf0)) 4 12))
           [1 1 1 1 1 1 1 1]))
    (is (= (bit-and (b8 0xf) 0x3c) (b8 0xc)))
    (is (= (bit-or 0xf0 (b8 0xf)) (b8 0xff)))
    (is (= (bit-xor (instance (uintm 8) 0x3c) (b8 0xf))
           (b8 0x33))))
  (is (= (get-bits true) (promote (bits 1) 1)))
  (is (= (get-bits false) (promote (bits 1) 0)))
  (is (= (count (value (get-bits
                         (promote (enum #{:a :b}) :a))))
         1)) 
  (is (= (count (value (get-bits
                         (promote (enum #{:a :b :c}) :a))))
         2)))  

(deftest sim-uintm-bits-test
  (let [mod (module [:outputs [c (promote (uintm 8) 0)
                               d (promote (bits 8) 0)]]
                    (connect c (+ c 1))
                    (connect d (bit-slice (get-bits c) 0 4)))
        sim (make-sim mod)
        init-state (first sim)
        init-fns (second sim)]
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [(:token mod) :c])
           (instance (uintm 8) 10))
        "ran and counted up to 10")
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [(:token mod) :d])
           (instance (bits 4) [1 0 0 1]))
        "ran and counted up to 10 bits")
    (is (= (get (exec-sim init-state
                          init-fns
                          18)
                [(:token mod) :d])
           (instance (bits 4) [0 0 0 1]))
        "ran and counted up to 10 bits")))

(deftest sim-cast-test
  (let [mod (module [:outputs [odd false
                               c (promote (uintm 3) 0)]]
                    (let [new-c (inc c)]
                      (connect odd (cast (anontype :boolean) 
                                         (bit-slice (get-bits new-c) 0 1)))
                      (connect c new-c)))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 1)
                [(:token mod) :c])
           (promote (uintm 3) 1)))

    (is (= (get (exec-sim state fns 1)
                [(:token mod) :odd])
           true))
    (is (= (get (exec-sim state fns 2)
                [(:token mod) :odd])
           false))
    (is (= (get (exec-sim state fns 3)
                [(:token mod) :odd])
           true))

    (is (= (get (exec-sim state fns 30)
                [(:token mod) :c])
           (instance (uintm 3) 30 :constrain)))
    (is (= (get (exec-sim state fns 30)
                [(:token mod) :odd])
           false))))

(deftest sim-mux2-test
  (let [mod (module [:outputs [flip false]]
                    (connect flip (mux2 flip
                                        false
                                        true)))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 0)
                [(:token mod) :flip])
           false))
    (is (= (get (exec-sim state fns 1)
                [(:token mod) :flip])
           true))
    (is (= (get (exec-sim state fns 2)
                [(:token mod) :flip])
           false))
    (is (= (get (exec-sim state fns 10)
                [(:token mod) :flip])
           false))
    (is (= (get (exec-sim state fns 11)
                [(:token mod) :flip])
           true))))

(deftest sim-equals-test
  (let [mod (module [:outputs [triggered false]
                     :feedback [c ((uintm 2) 0)]]
                    (let [c' (inc c)]
                      (connect c c')
                      (connect triggered (pr-trace "bool" (= c' ((uintm 2) 3))))))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 0)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 1)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 2)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 3)
                [(:token mod) :triggered])
           true))
    (is (= (get (exec-sim state fns 4)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 5)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 6)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 7)
                [(:token mod) :triggered])
           true))))

(deftest enum-initialize-test
  (let [e (enum #{:a :b :c})]
    (enum (:keymap e)))
  (is (thrown? ExceptionInfo
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 2) 0)})))
  (is (thrown? ExceptionInfo 
               (enum {:a (cast (bits 2) 0)
                      2 (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo 
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 3) 1)})))
  (is (thrown? ExceptionInfo 
               (enum {:a 0
                      :b (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo 
               (enum #{:a 1 "a"}))))

(deftest bundle-test
  (let [b1 (bundle {:a (uintm 3)
                    :b (enum #{:foo :bar})})]
    (is (instance b1 {:a (cast (uintm 3) 2)
                      :b (cast (enum #{:foo :bar})
                               :foo)}))
    (is (thrown? ExceptionInfo
                 (instance b1 {:a 1
                               :b (cast (enum #{:foo :bar})
                                        :foo)})))
    (is (= (:a (cast b1 {:a 4 :b :bar})) ((uintm 3) 4)))
    (let [{:keys [a b]} (cast b1 {:a 2 :b :bar})]
      (is (= a ((uintm 3) 2)))
      (is (= b ((enum #{:foo :bar}) :bar))))
    (let [mod (module [:outputs [o (cast b1 {:a 0 :b :foo})]]
                      (let [{:keys [a b]} o
                            a' (inc a)
                            b' (mux2 (= b :foo)
                                     :bar :foo)]
                        (connect o (cast b1 {:a a' :b b'}))))
          [state fns] (make-sim mod)] 
      (is (= (get (exec-sim state fns 0)
                  [(:token mod) :o])
             (cast b1 {:a 0 :b :foo})))
      (is (= (get (exec-sim state fns 1)
                  [(:token mod) :o])
             (cast b1 {:a 1 :b :bar})))
      (is (= (get (exec-sim state fns 2)
                  [(:token mod) :o])
             (cast b1 {:a 2 :b :foo}))))
    (let [mod (module [:outputs [o (cast b1 {:a 0 :b :foo})]]
                      (let [a' (inc (bundle-get o :a))
                            b' (mux2 (= (bundle-get o :b)
                                        (cast (enum #{:foo :bar})
                                              :foo))
                                     :bar :foo)]
                        (connect o (cast b1 {:a a' :b b'}))))
          [state fns] (make-sim mod)] 
      (is (= (get (exec-sim state fns 0)
                  [(:token mod) :o])
             (cast b1 {:a 0 :b :foo})))
      (is (= (get (exec-sim state fns 1)
                  [(:token mod) :o])
             (cast b1 {:a 1 :b :bar})))
      (is (= (get (exec-sim state fns 2)
                  [(:token mod) :o])
             (cast b1 {:a 2 :b :foo})))
      ))) 
