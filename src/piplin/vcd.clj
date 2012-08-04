(ns piplin.vcd
  (:use [clojure.string :only [join]])
  (:use [piplin.types :only [typeof value]]) 
  (:use [clojure.java.shell :only [sh]])
  (:use [clojure.java.io :only [file]])
  (:use [piplin.types.bits :only [bit-width-of serialize]]))

;VCD support for piplin

(defn next-vcd-id
  [id]
  (loop [id id out [] carry true]
    (if-not carry
      (concat out id)
      (if (empty? id)
        (recur [33] out false)
        (let [x (inc (first id))
              carry (= x 127)
              x (if carry 33 x)]
          (recur (rest id) (conj out x) carry))))))

(defn vcd-id->str
  [id]
  (join (map char id)))

(defn- dump-cycle
  "Takes a map from keys to piplin serializable
  objects and a key->shortname mapping and returns
  a vcd-style dump."
  [cycle-map short-map]
  (join (map (fn [[k v]]
               (let [bits (value (serialize v))]
                 (str
                   (if (> (count bits) 1)
                     (str \b 
                          (join bits) 
                          \ )
                     (first bits))
                   (short-map k)
                   "\n")))
             cycle-map))) 

(defn trace->vcd
  "Takes a seq of maps of values at each cycle and
  returns a string with that data in VCD format."
  [trace]
  (let [date (str (java.util.Date.))
        ver "Piplin VCD Dumper"
        names (atom {:clock \!})
        gen-name (atom [34])]
    (doseq [[k iv] (first trace)]
      (swap! names assoc k (vcd-id->str @gen-name))
      (swap! gen-name next-vcd-id))
    (str "$date\n  " date "\n$end\n"
         "$version\n  " ver "\n$end\n"
         "$timescale 1ns $end\n"
         "$scope module logic $end\n"
         "$var wire 1 ! clock $end\n"
         (join (map (fn [[k v]]
                      (str
                        "$var wire "
                        (bit-width-of (typeof v)) " "
                        (@names k) " "
                        (join \_ (map name k)) " $end\n"))
                    (first trace)))
         "$upscope $end\n"
         "$enddefinitions $end\n"
         "$dumpvars\n"
         "0!\n"
         (dump-cycle (first trace) @names)
         "$end\n"
         (join (map #(str
                       \# %2 "\n"
                       "0!\n"
                       \# (inc %2) "\n"
                       "1!\n"
                       (dump-cycle %1 @names))
                    (rest trace)
                    (iterate #(+ % 2) 1)))
         ;padding cycle
         \# (* 2 (count trace)) "\n"
         "0!\n"
         )))

(defn spit-trace
  "Writes the trace to the file with the
  given name (path is a String)"
  [path trace]
  (spit (file path) (trace->vcd trace)))

(defn trace->gtkwave
  "Opens the given trace in GTKWave"
  [trace]
  (spit-trace "piplin_trace.vcd" trace)
  (sh "gtkwave" "piplin_trace.vcd"))
