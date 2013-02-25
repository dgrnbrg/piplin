(ns piplin.vga
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp or and bit-shift-right bit-shift-left])
  (:use piplin.core
        [piplin.util :only [let']]))

;This generates xvga display signals (1024 x 768 @ 60 Hz)
;Requires a 65MHz clock
(defmodule xvga
  []
  [:outputs [hcount ((uintm 11) 0)
             vcount ((uintm 10) 0)
             hsync false
             vsync false
             blank false]
   :feedback [hblank false
              vblank false]]
  (let' [
        ;Horizontal: 1344 pixels total
        ;display 1024 pixels per line
        hblankon (= hcount 1023)
        hsyncon (= hcount 1047)
        hsyncoff (= hcount 1183)
        hreset (= hcount 1343)

        ;Vertical: 806 lines total
        ;display 768 lines
        vblankon (and hreset
                      (= vcount 767))
        vsyncon (and hreset
                     (= vcount 776)) 
        vsyncoff (and hreset
                      (= vcount 782)) 
        vreset (and hreset
                    (= vcount 805)) 

        ;Sync and blanking
        hblank' (cond
                  hreset false
                  hblankon true
                  :else hblank)
        vblank' (cond
                  vreset false
                  vblankon true
                  :else vblank)]

    (connect hcount (mux2 hreset ((uintm 11) 0) (inc hcount)))
    (connect hblank hblank')
    (connect hsync (cond
                     hsyncon false
                     hsyncoff true
                     :else hsync)) ;Active low

    (connect vcount (cond
                      (not hreset) vcount
                      vreset ((uintm 10) 0)
                      :else (inc vcount)))
    (connect vblank vblank')
    (connect vsync (cond
                     vsyncon false
                     vsyncoff true 
                     :else vsync))

    (connect blank (or vblank'
                       (and hblank'
                            (not hreset))))))

;(spit "xvga.v" (modules->all-in-one (xvga)))
