(ns piplin.test.z80
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or bit-shift-left bit-shift-right])
  (:use piplin.core)
  (:use clojure.test)
  (:use piplin.z80))

(deftest exec-test
  (is (exec-uinstr (cast microinstruction
                        {:imm {:imm ((uintm 16) 3)
                               :dst :5}})
                  nil
                  nil)
      (cast writeback
            {:reg {:reg :5 :data ((uintm 16) 22)}}))
  (is (exec-uinstr (cast microinstruction
                         {:store {:src-reg :1
                                  :dst-addr :5}})
                   {(uregister :1) ((uintm 16) 22)
                    (uregister :5) ((uintm 16) 44)}
                   nil)
      (cast writeback
            {:mem {:mem ((uintm 16) 44) :data ((uintm 16) 22)}}))
  (is (exec-uinstr (cast microinstruction
                         {:load {:src-addr :2
                                 :dst-reg :4}})
                   {(uregister :2) ((uintm 16) 22)}
                   {((uintm 16) 22)
                    ((uintm 16) 88)})
      (cast writeback
            {:reg {:reg :4 :data ((uintm 16) 88)}})))
