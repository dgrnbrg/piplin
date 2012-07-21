(ns piplin.seven-segment-decoder
  (:use clojure.test)
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :as clj :exclude [not= bit-or cond bit-xor + - * bit-and assoc assoc-in inc dec bit-not condp < > <= >= = cast get not])
  (:use [piplin.types bits boolean bundle enum numbers union core-impl binops uintm])
  (:use [piplin types math modules sim connect mux]))

(defn log16
  "log base 16"
  [x]
  (/ (Math/log x)
     (Math/log 16)))

(declare decode-digit)

(defn decoder
  "Constructs a module that decodes its input
  into 7 segment display controls. `bit-width`
  is the width of the input, and mapping is a
  map from keywords in `#{:top, :bottom, :upper-left
  :upper-right, :lower-left, :lower-right, :middle}`
  to indices into the 7 bit output. This allows
  whatever encoding scheme is needed on the output.
  
  E.x. for
     _______
    |   0   |
    |1     5|
    |   6   |
    |______ |
    |       |
    |2     4|
    |   3   |
    |-------|

  the mapping would be
  
  {:top 0
  :upper-left 1
  :lower-left 2
  ...
  }
  "
  [bit-width mapping]
  (let [max-value (Math/pow 2 bit-width)
        required-digits (int (Math/ceil (log16 max-value)))
        padding-needed (mod (- 4 bit-width) 4)]
    (module [:inputs [in (bits bit-width)]
             :outputs [out (cast (bits (* 7 required-digits)) 0)]]
            (let [in-padded (bit-cat
                              (cast (bits padding-needed) 0)
                              in)
                  slices (for [low (range 0 bit-width 4)]
                           (bit-slice in-padded low (+ low 4)))
                  digits (map #(decode-digit % mapping) slices)]
              (connect out (apply bit-cat
                                  (reverse digits)))))))

(def sample-mapping
  {:top 0
   :upper-left 1
   :lower-left 2
   :bottom 3
   :lower-right 4
   :upper-right 5
   :middle 6})

(defn show-pattern-bits
  "Takes a set of elements to turn on and
  returns the bit pattern to turn on those
  elements"
  [elts mapping]
  (reduce (fn [pattern position]
            (bit-or (cast (bits 7) (bit-shift-left 1 (mapping position)))
                    pattern))
          #b000_0000
          elts))

(def decoder-mapping
  {#b0000 #{:top :upper-left :lower-left
            :upper-right :lower-right :bottom} 

   #b0001 #{:upper-right :lower-right} 

   #b0010 #{:top :upper-right :middle
            :lower-left :bottom} 

   #b0011 #{:top :middle :bottom
            :upper-right :lower-right} 

   #b0100 #{:upper-left :upper-right
            :middle :lower-right} 

   #b0101 #{:top :upper-left :middle
            :lower-right :bottom} 

   #b0110 #{:top :upper-left :middle
            :lower-left :lower-right :bottom} 

   #b0111 #{:top :upper-right :lower-right} 

   #b1000 #{:top :upper-left :lower-left :middle
            :upper-right :lower-right :bottom} 

   #b1001 #{:top :upper-left :middle
            :upper-right :lower-right} 

   #b1010 #{:top :upper-left :middle :lower-left
            :upper-right :lower-right} 

   #b1011 #{:upper-left :lower-left :middle
            :lower-right :bottom} 

   #b1100 #{:upper-left :lower-left
            :top :bottom} 

   #b1101 #{:upper-right :lower-left :middle
            :lower-right :bottom} 

   #b1110 #{:upper-left :lower-left
            :middle :top :bottom} 

   #b1111 #{:upper-left :lower-left
            :middle :top}})

;# Checking we made no typos
;How do we know there are no mistakes in the mapping above?
;Here is a simple function, seven-seg-to-ascii, that
;takes a set of regions to turn on, and prints those regions
;in ascii. We can visually check that we made to mistakes with
;the following snippet:
;
;    (dotimes [i 16]
;      (println (str "[" i "]\n"))
;      (println (-> (cast (bits 4) i)
;                 decoder-mapping
;                 seven-seg-to-ascii)))

(defn decode-digit
  "Takes 4 bits as the input, along with the
  mapping, and returns the bit pattern to display
  that number in hex."
  [in mapping]
  (letfn [(decode [in]
            (-> (decoder-mapping in)
              (show-pattern-bits mapping)))]
    (condp = in
      #b0000 (decode #b0000)
      #b0001 (decode #b0001)
      #b0010 (decode #b0010)
      #b0011 (decode #b0011)
      #b0100 (decode #b0100)
      #b0101 (decode #b0101)
      #b0110 (decode #b0110)
      #b0111 (decode #b0111)
      #b1000 (decode #b1000)
      #b1001 (decode #b1001)
      #b1010 (decode #b1010)
      #b1011 (decode #b1011)
      #b1100 (decode #b1100)
      #b1101 (decode #b1101)
      #b1110 (decode #b1110)
      ;#b1111
      (decode #b1111))))

(defn seven-seg-to-ascii
  "Takes a set of elements to turn on and
  returns an ascii representation"
  [pattern]
  (letfn [(vert [b]
            (if (pattern b) "|" " "))
          (horiz [b]
            (if (pattern b) "----" "    "))]
    (str
      " " (horiz :top) " \n"
      (vert :upper-left) (horiz ::nil) (vert :upper-right) "\n"
      " " (horiz :middle) " \n"
      (vert :lower-left) (horiz ::nil) (vert :lower-right) "\n"
      " " (horiz :bottom) " \n"
      )))

;TODO: pr-trace should probably put trace arg first, format second
;also, pr-trace prints way too many times per cycle, and it conds,
;it shows me every branch, not just the true branch

(defn seven-seg-tester
  [bit-width]
  (let [deco (decoder bit-width sample-mapping)]
    (module tb [:feedback [x ((uintm bit-width) 0)]
                :outputs [x_out (cast (typeof (subport deco :deco :out)) 0)]
                :modules [deco deco]]
            (connect x (inc x))
          (connect x_out deco$out)
          (connect
            deco$in
            (serialize x)))))
