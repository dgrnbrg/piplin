(ns piplin.test.mips
  (:require [piplin.math :as h])
  (:use [piplin.mips])
  (:use [clojure.test])
  (:refer-clojure :as clj))

(deftest decode-add-imm
  (let [decoded (decode (str-to-bits "001001_00001_00010_1000_0000_0000_0000"))]
    (is (h/= (h/bundle-get decoded :op)
             (alu-op :add)))
    (is (h/= (h/bundle-get decoded :y)
             (reg-or-imm
               {:imm (u32m 32768)})))
    (is (h/= (h/bundle-get decoded :x)
             (reg-or-imm
               {:reg (reg :23)})))
    (is (h/= (h/bundle-get decoded :dst)
             (reg :22)))))
