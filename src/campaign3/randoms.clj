(ns campaign3.randoms)

(defmulti ^:private randoms-preset (comp keyword :preset))

(defn- rand-from [vs]
  #(rand-nth vs))

(defn randoms->fn [randoms]
  (cond
    (vector? randoms) (apply juxt (map (comp rand-from randoms-preset) randoms))
    (map? randoms) (randoms-preset randoms)))

(defmethod randoms-preset :skills [{:keys [type]
                                    :or   {type "all"}}]
  (case type
    "common" ["perception" "medicine" "deception" "persuasion" "investigation" "insight" "survival"]
    "uncommon" ["arcana" "athletics" "acrobatics" "sleight of hand" "stealth" "history"
                "nature" "religion" "animal handling" "intimidation" "performance"]
    "all" ["perception" "medicine" "deception" "persuasion" "investigation" "insight" "survival"
           "arcana" "athletics" "acrobatics" "sleight of hand" "stealth" "history"
           "nature" "religion" "animal handling" "intimidation" "performance"])) ;TODO ensure none were missed

(defmethod randoms-preset :range [{:keys [args]}]
  (vec (apply range args)))

(defmethod randoms-preset :damage-types [{:keys [type]
                                          :or   {type "all"}}]
  (case type
    "physical" ["bludgeoning" "piercing" "slashing"]
    "non-physical" ["acid" "cold" "fire" "force" "lightning" "necrotic" "poison" "psychic" "radiant" "thunder"]
    "elemental" ["cold" "fire" "lightning"]
    "all" ["acid" "bludgeoning" "cold" "fire" "force" "lightning" "necrotic" "piercing" "poison" "psychic" "radiant" "slashing" "thunder"]))

(defmethod randoms-preset :attributes [{:keys [type]
                                        :or   {type "all"}}]
  (case type
    "common" ["Constitution" "Dexterity" "Wisdom"]
    "uncommon" ["Strength" "Intelligence" "Charisma"]
    "all" ["Charisma" "Constitution" "Dexterity" "Intelligence" "Strength" "Wisdom"]))

(defmethod randoms-preset :monster-type [_]
  ["Abberation" "Beast" "Celestial" "Construct" "Dragon" "Elemental" "Fey"
   "Fiend" "Giant" "Humanoid" "Monstrosity" "Ooze" "Plant" "Undead"])

(defmethod randoms-preset :cantrips [_]
  ["Acid Splash", "Blade Ward", "Booming Blade", "Chill Touch", "Control Flames",
   "Create Bonfire", "Dancing Lights", "Druidcraft", "Eldritch Blast",
   "Encode Thoughts", "Fire Bolt", "Friends", "Frostbite", "Green-Flame Blade",
   "Guidance", "Gust", "Infestation", "Light", "Lightning Lure", "Mage Hand",
   "Magic Stone", "Mending", "Message", "Mind Sliver", "Minor Illusion",
   "Mold Earth", "Poison Spray", "Prestidigitation", "Primal Savagery",
   "Produce Flame", "Ray of Frost", "Resistance", "Sacred Flame", "Sapping Sting",
   "Shape Water", "Shillelagh", "Shocking Grasp", "Spare the Dying", "Sword Burst",
   "Thaumaturgy", "Thorn Whip", "Thunderclap", "Toll the Dead", "True Strike",
   "Vicious Mockery", "Word of Radiance"]) ;TODO update list, generify for level x spell, school?

(defmethod randoms-preset :without-replacement [{:keys [amount from]}]
  (let [vs (randoms-preset from)]
    (comment "sample amount from vs")))
