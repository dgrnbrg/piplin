(ns piplin.test.mips
  (:require [piplin.math :as h])
  (:use [piplin.mips])
  (:use [clojure.test])
  (:refer-clojure :as clj :exclude [not= bit-or cond bit-xor + - * bit-and assoc assoc-in inc dec bit-not condp < > <= >= = cast get not]))

(deftest decode-add-imm
  (let [decoded (decode #b001001_00001_00010_1000_0000_0000_0000)]
    (is (h/= (h/get decoded :op)
             (alu-op :add)))
    (is (h/= (h/get decoded :y)
             (reg-or-imm
               {:imm (u32m 32768)})))
    (is (h/= (h/get decoded :x)
             (reg-or-imm
               {:reg (reg :23)})))
    (is (h/= (h/get decoded :dst)
             (reg :22)))))
