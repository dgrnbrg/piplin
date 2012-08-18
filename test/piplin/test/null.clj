(ns piplin.test.null
  (:refer-clojure :exclude [cast])
  (:use clojure.test)
  (:use [piplin protocols types])
  (:use [piplin.types bits null]))

(deftest serdes-nil
  (is (= (deserialize (anontype :null) (serialize nil)) nil)))

(deftest nil-piplin-type
  (is (pipinst? nil)))
