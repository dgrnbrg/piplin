(ns piplin.test.mips
  (:use [piplin.types
         [binops :only [=]]])
  (:use [piplin.mips])
  (:use [clojure.test])
  (:refer-clojure :as clj :exclude [=]))

(deftest decode-add-imm
  (let [decoded (decode #b001001_00001_00010_1000_0000_0000_0000)]
    (is (= (get decoded :op)
             (alu-op :add)))
    (is (= (get decoded :y)
             (reg-or-imm
               {:imm (u32m 32768)})))
    (is (= (get decoded :x)
             (reg-or-imm
               {:reg (reg :23)})))
    (is (= (get decoded :dst)
             (reg :22)))))
