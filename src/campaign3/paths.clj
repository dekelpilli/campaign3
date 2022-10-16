(ns campaign3.paths
  (:require (campaign3
              [db :as db]
              [helmets :as helmets]
              [prompting :as p]
              [util :as u])))

(def divinity-paths (->> (db/load-all :divinity-paths)
                         (u/assoc-by :name)))

(defn- new-path-progress [character]
  (when-let [{:keys [name]} (p/>>item "New path:" divinity-paths)]
    {:progress  0
     :path      name
     :character character}))

(defn path-descriptions []
  (->> (vals divinity-paths)
       (mapv (fn [{:keys [name info]}]
               (format "%s: %s" name info)))))

(defn use-dust []
  (u/when-let* [character (p/>>item "Character:" (keys helmets/character-enchants))
                {:keys [path progress] :as current-path} (-> (db/execute! {:select [:*]
                                                                           :from   [:divinity-progress]
                                                                           :where  [:< :progress 5]})
                                                             first
                                                             (or (new-path-progress character)))]
    (db/execute! {:insert-into   :divinity-progress
                  :values        [(update current-path :progress inc)]
                  :on-conflict   [:character :path]
                  :do-update-set {:progress :EXCLUDED.progress}})
    {:modifier (get-in divinity-paths [path :levels progress])
     :tier     (inc progress)}))
