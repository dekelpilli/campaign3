(ns campaign3.riddle
  (:require
    [campaign3
     [util :as util]]))

(defn new! []
  #_(let [{:keys [riddle] :as randomised-riddle} (->> @riddles
                                                    (util/rand-enabled)
                                                    (util/fill-randoms))]
    (->> @riddles
         (map (fn [r] (if (= (:riddle r) riddle)
                        (assoc randomised-riddle :enabled? false)
                        r)))
         (override-riddles!))
    randomised-riddle))
