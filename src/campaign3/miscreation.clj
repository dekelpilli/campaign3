(ns campaign3.miscreation
  (:require
    [campaign3
     [util :as util]
     [state :refer [miscreations]]]))

(defn new []
  (util/get-multiple-items @miscreations #(inc (rand-int 3))))
