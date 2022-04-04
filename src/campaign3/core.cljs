(ns campaign3.core
  (:require [campaign3.io :as io]))

(defn -main [& _args]
  (js/console.log "what!?!?")
  #_(let [s (io/select "what do you want to do?" {"leave" ":("
                                                  "stay"  ":)"})]
      (js/console.log s)))

(set! *main-cli-fn* -main)

(defn r [& _]
(js/console.log "Started."))