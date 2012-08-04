(ns piplin.connect
  (:refer-clojure :exclude [cast])
  (:use [piplin.types])
  (:use [slingshot.slingshot]))

(defn connect
  {:dynamic true}
  [reg expr]
  (if (:token (value reg))
    (throw+ (error "Must call connect within a module"))
    (throw+ (error "Must connect to a register"))))
