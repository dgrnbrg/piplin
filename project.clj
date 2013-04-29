(defproject piplin "0.1.2-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.logic "0.6.7"]
                 [slingshot "0.10.1"]
                 [prismatic/plumbing "0.0.1"]
                 [swiss-arrows "0.5.1"]
                 [org.clojure/core.incubator "0.1.1"]
                 [org.clojure/tools.macro "0.1.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/algo.generic "0.1.0"]]

  :profiles {:1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :dev {:plugins [[lein-marginalia "0.7.0"]
                             [codox "0.6.4"]]
                   :codox {:sources ["src"]
                           :exclude piplin.mips
                           :src-dir "https://github.com/dgrnbrg/piplin/blob/master"
                           :src-linenum-anchor-prefix "L"}}}

  :aliases {"all" ["with-profile" "dev:dev,1.5"]}

  :min-lein-version "2.0.0"
  :source-paths ["src" "examples"]
  :description "A tool for programming FPGAs")
