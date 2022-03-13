(ns campaign3.core
  (:require [cljs.nodejs :as nodejs]
            [campaign3.io :as io]))

(nodejs/enable-util-print!)

(defn -main [& _args]
  (js/console.log (io/input "what!?!?"))
  #_(let [s (io/select "what do you want to do?" {"leave" ":("
                                                "stay"  ":)"})]
    (js/console.log s)))

(set! *main-cli-fn* -main)
