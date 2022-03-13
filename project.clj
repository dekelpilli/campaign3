(defproject myproject "0.5.0-SNAPSHOT"
  :description "Campaign #3"
  :dependencies [[org.clojure/clojurescript "1.11.4"]
                 [org.clojure/clojure "1.10.3"]
                 [org.clojure/core.async "1.5.648"]
                 [funcool/promesa "8.0.450"]]
  :plugins [[lein-cljsbuild "1.1.8"]]
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler     {:output-to     "target/js/campaign3.js"
                                       :npm-deps      {:enquirer "2.3.6"}
                                       :target        :nodejs
                                       :optimizations :simple}}]})
