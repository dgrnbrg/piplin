(ns piplin.docs
  (:refer-clojure :exclude [replace])
  (:use [clojure.string :only [join replace]]
        [plumbing.core :only [fnk]]))


(defn get-var-info
  [& var-syms]
  (doseq [vs var-syms]
    (require (symbol (namespace vs))))
  (map (fn [sym]
         (let [data (-> sym resolve meta)]
           (if-let [protocol (:protocol data)]
             (merge (meta protocol) data)
             data
             )
           )
         ) var-syms))

;(clojure.pprint/pprint (get-var-info 'piplin.types/uninst))
;(clojure.pprint/pprint (get-var-info 'piplin.types/uninst))

(defn escape
  [s]
  (-> s
      (replace "&" "&nbsp;")
      (replace "<" "&lt;")
      ;(replace "*" "\\*")
      )
  )

(def my-rev
  (.trim (:out (clojure.java.shell/sh "git" "rev-parse" "HEAD")))
  )

(defn generate-section
  [title & var-syms]
  (println (remove :line (apply get-var-info var-syms)))
  (str "## " title "\n"
       (->> (apply get-var-info var-syms)
            (map (fn [{:keys [ns name] :as r}]
                   (cond
                     (and (= ns 'piplin.protocols)
                          (= name 'typeof))
                     (assoc r :file "piplin/protocols.clj" :line 5)
                     :else
                     r
                     )
                   ))
            (mapcat (fnk [{arglists []} ns name {doc ""} line file]
                         (let [anchor (gensym)]
                           `(~(str "### <a href='https://github.com/dgrnbrg/piplin/blob/" my-rev "/src/" file "#L" line "'>" (escape ns) "/" (escape name)  "</a>")
                             ""
                             ~@(when (seq arglists) [(str "<code>" (join " " (map escape arglists)) "</code>")])
                             ""
                             ~(escape doc)
                             ""
                             ;"<script>"
                             ;~(str "index.add({id: '" anchor "', title: '" ns \space name "', body: '" (replace doc "'" "\\'") "' })")
                             ;~(str "ref_back['" anchor "'] = '" ns \/ name "'")
                             ;"</script>"
                             ;~(str "[Go to source](https://github.com/dgrnbrg/piplin/blob/master/src/" file "#L" line ")")
                             ""
                             ))


                         )
                    )
            (join "\n")
            ))
  )

(spit "docs.md" (str
                  ;"CSS: http://twitter.github.io/bootstrap/assets/css/bootstrap.css\n\n"
                  ;"<script src=\"http://code.jquery.com/jquery-1.9.1.min.js\"> \n"
                  ;"</script>\n\n"
                  ;"<script src=\"http://twitter.github.io/bootstrap/assets/js/bootstrap-typeahead.js\"> \n"
                  ;"</script>\n\n"
                  ;"<script src=\"lunr.js\"> \n"
                  ;"</script>\n\n"
                  ;"<script>\n"
                  ;"var index = lunr(function () { this.field('title', {boost: 10}); this.field('body'); this.ref('id'); })\n"
                  ;"var ref_back = {}"
                  ;"</script>\n\n"
                  ;"<input type='text' data-provide='typeahead' autocomplete='off' id='typeahead'/>\n\n"
                  "---\n"
                  "title: Documentation\n"
                  "layout: article\n"
                  "---\n"

                  (generate-section
                    "Types"
                    'piplin.protocols/typeof
                    'piplin.core/kindof
                    'piplin.core/anontype
                    'piplin.core/cast
                    )

                  (generate-section
                    "Modules"
                    'piplin.core/modulize
                    'piplin.core/compile-root
                    'piplin.core/input
                    )

                  (generate-section
                    "Simulation"
                    'piplin.core/sim
                    'piplin.core/spit-trace
                    'piplin.core/trace->gtkwave
                    )

                  (generate-section
                    "Verilog"
                    'piplin.core/->verilog
                    'piplin.core/verify
                    )

                  (generate-section
                    "Bits"
                    'piplin.core/bits
                    'piplin.core/bit-width-of
                    'piplin.core/serialize
                    'piplin.core/deserialize
                    'piplin.core/bit-cat
                    'piplin.core/bit-slice
                    'piplin.core/bit-and
                    'piplin.core/bit-or
                    'piplin.core/bit-xor
                    'piplin.core/bit-not
                    'piplin.core/bit-shift-left
                    'piplin.core/bit-shift-right
                    )

                  (generate-section
                    "Boolean Logic/Equality"
                    'piplin.core/=
                    'piplin.core/not=
                    'piplin.core/and
                    'piplin.core/or
                    'piplin.core/not
                    )

                  (generate-section
                    "Conditionals"
                    'piplin.core/mux2
                    'piplin.core/cond
                    'piplin.core/condp
                    )

                  (generate-section
                    "Math Ops"
                    'piplin.core/+
                    'piplin.core/-
                    'piplin.core/*
                    'piplin.core/inc
                    'piplin.core/dec
                    'piplin.core/<
                    'piplin.core/<=
                    'piplin.core/>
                    'piplin.core/>=
                    'piplin.core/zero?
                    'piplin.core/pos?
                    'piplin.core/neg?
                    )

                  (generate-section
                    "Numeric Types"
                    'piplin.core/uintm
                    'piplin.core/sints
                    'piplin.core/sign-extend
                    'piplin.core/sfxpts
                    'piplin.core/complex
                    'piplin.core/real-part
                    'piplin.core/imag-part
                    )

                  (generate-section
                    "Arrays"
                    'piplin.core/array
                    'piplin.core/store
                    'clojure.core/assoc
                    'clojure.core/get
                    )

                  (generate-section
                    "Bundles"
                    'piplin.core/bundle
                    'clojure.core/assoc
                    'clojure.core/update-in
                    'clojure.core/get
                    )

                  (generate-section
                    "Enums and Unions"
                    'piplin.core/enum
                    'piplin.core/union
                    'piplin.core/union-match
                    )

                  (generate-section
                    "Other"
                    'piplin.core/log2
                    'piplin.types/uninst
                    )

                  ;"<script>\n"
                  ;"var order = index.search('uni');"
                  ;""
                  ;"window.jQuery('#typeahead').typeahead({'source': (function (query, cb) {ref_back[index.search(query)[0].re]})})\n"
                  ;"</script>\n\n"
                  ))

(println (slurp "docs.md"))

(println (:err (clojure.java.shell/sh "maruku" "test.md")))
