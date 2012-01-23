(ns piplin.verilog-gen
  [:use fleet])


(def uintm-add-template
  (fleet [bits x y out]
         "uintm_add #(<(str bits)>)
           <(gensym \"uintm_add\")>(
             .x(<(str x)>),
             .y(<(str y)>),
             .out(<(str out)>));"))

;example
(str (uintm-add-template 1 2 3 4))

