(ns piplin.connect
  (:refer-clojure :exclude [cast])
  (:use [piplin.types])
  (:use [slingshot.slingshot]))

(defn connect
  {:dynamic true}
  [reg expr]
  (if (:token reg)
    (throw+ (error "Must call connect within a module"))
    (throw+ (error "Must connect to a register"))))

(defn connect-impl
  "This connects a register to an expr"
  [reg expr]
  (when-not (#{:register :subport} (:port-type (value reg)))
      (throw+ (error "Must be :register or :subport, was"
                     (:port-type (value reg)))))  
  {:type (:port-type (value reg))
   :args {:reg reg 
          :expr (cast (typeof reg) expr)}})
 
