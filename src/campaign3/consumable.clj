(ns campaign3.consumable
  (:require
    [campaign3
     [util :as util]
     [state :refer [consumables]]]))

(defn new []
  (util/get-multiple-items @consumables #(inc (rand-int 4))))
