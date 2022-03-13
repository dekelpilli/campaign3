(require 'cljs.build.api)

(cljs.build.api/build "src"
                      {:main      'campaign3.core
                       :output-to "main.js"
                       :target    :nodejs})
