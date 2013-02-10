(ns piplin.nexys6.sw-led
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp and or])
  (:use piplin.core)
  (:use [piplin.vga :only [xvga]])
  (:use [piplin.seven-segment-decoder :only [decoder]]))

(def seven-seg-map
  {:top 0
   :upper-left 5
   :lower-left 4
   :bottom 3
   :lower-right 2
   :upper-right 1
   :middle 6})

(defmodule led->switch []
  [:outputs [led #b0000_0000
             seven_seg_cathode #b0000_000_0
             seven_seg_anode #b0000
             vgaRed #b000
             vgaGreen #b000
             vgaBlue #b00
             Hsync false
             Vsync false]
   :inputs [sw (bits 8)]
   :feedback [timer ((uintm 24) 1)
              counter ((uintm 16) 0)
              faster-counter ((uintm 14) 0)
              video-buffer (cast (array (bits 1)
                                        (int (Math/pow 2 14)))
                                 (take (int (Math/pow 2 14))
                                       (interleave (repeat #b0)
                                                   (repeat #b1))))]
   :modules [deco (decoder 16 seven-seg-map)
             vga (xvga)]]
  (connect Hsync vga$hsync)
  (connect Vsync vga$vsync)
  (connect (get video-buffer (serialize faster-counter))
           (bit-not (get video-buffer (serialize faster-counter))))
  (let [hcount (serialize vga$hcount)
        border? (or (= vga$hcount 0)
                    (= vga$hcount 1023)
                    (= vga$vcount 0)
                    (= vga$vcount 767))
        r (condp = (bit-slice sw 0 2)
            #b00 (bit-slice hcount 8 9)
            #b01 (mux2 border? #b1 #b0)
            ;else, do buffered image
            (get video-buffer (bit-cat
                                (bit-slice (serialize vga$vcount) 1 8)  
                                (bit-slice (serialize vga$hcount) 1 8)))
            
            )
        g (condp =  (bit-slice sw 0 2)
            #b00 (bit-slice hcount 7 8)
            #b01 (mux2 border? #b1 #b0)
            ;else, do buffered image
            (get video-buffer (bit-cat
                                (bit-slice (serialize vga$vcount) 1 8)  
                                (bit-slice (serialize vga$hcount) 1 8)))
            )
        b (condp = (bit-slice sw 0 2)
            #b00 (bit-slice hcount 6 7)
            #b01 (mux2 border? #b1 #b0)
            ;else, do buffered image
            (get video-buffer (bit-cat
                                (bit-slice (serialize vga$vcount) 1 8)  
                                (bit-slice (serialize vga$hcount) 1 8)))
            )]
    (connect vgaRed (bit-cat r r r))
    (connect vgaGreen (bit-cat g g g)) 
    (connect vgaBlue (bit-cat b b)))
  (connect led sw)
  (connect timer (inc timer))
  (connect counter (mux2 (= 0 timer)
                         (inc counter)
                         counter))
  (connect faster-counter (mux2 (= 0 (deserialize
                                       (uintm 20)
                                       (bit-slice (serialize timer) 0 20)))
                                (inc faster-counter)
                                faster-counter))
  (connect deco$in (serialize counter))
  (let [
        ;Must multiplex in the millisecond regime 
        current-state (bit-slice
                        (serialize timer) 10 12)
        anode (condp = current-state
                #b00 #b1110
                #b01 #b1101
                #b10 #b1011
                ;#b11
                #b0111)
        cathode (condp = current-state
                  #b00 (bit-slice
                         deco$out 0 7)
                  #b01 (bit-slice
                         deco$out 7 14)
                  #b10 (bit-slice
                         deco$out 14 21)
                 ; #b11
                  (bit-slice
                         deco$out 21 28))]
    (connect seven_seg_anode anode)
    (connect seven_seg_cathode
             ;bit-not => active-low
             ;extra bit disables the decimal
             (bit-cat #b1 (bit-not cathode)))))

(spit "nexys-leds.v" (modules->all-in-one (led->switch)))
