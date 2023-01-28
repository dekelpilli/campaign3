(ns campaign3.randoms
  (:require [randy.core :as r]))

(defn- keyword-type [{:keys [type] :as conf}]
  (cond-> conf
          (some? type) (update :type keyword)))

(defmulti ^:private randoms-preset (comp keyword :preset))
(def ^:private random->values-vec (comp randoms-preset keyword-type))

(defmulti ^:private randoms-factor (comp keyword :preset))
(def ^:private random->weighting (comp randoms-factor keyword-type))

(defn- sample-fn [vs]
  (fn sample-random [] (r/sample vs)))

(defn- random->fn [random]
  (-> random random->values-vec sample-fn))

(defn randoms->fn [randoms]
  (cond
    (vector? randoms) (apply juxt (map random->fn randoms))
    (map? randoms) (random->values-vec randoms)))

(defn randoms->weighting-multiplier [randoms]
  (cond
    (vector? randoms) (transduce (map random->weighting) + 0 randoms)
    (map? randoms) (random->weighting randoms)
    (nil? randoms) 1))

(defmethod randoms-factor :languages [_] 2)
(defmethod randoms-preset :languages [_]
  ["Common" "Dwarvish" "Elvish" "Giant" "Gnomish" "Goblin" "Halfling" "Orc"
   "Abyssal" "Celestial" "Draconic" "Deep Speech" "Infernal" "Primordial" "Sylvan" "Undercommon"])

(defmethod randoms-factor :feats [_] 1)
(defmethod randoms-preset :feats [_]
  ["Alert" "Athlete" "Actor" "Blinktouched" "Brawler" "Charger" "Crossbow Expert" "Defensive Duelist" "Dual Wielder"
   "Dungeon Delver" "Durable" "Eldritch Adept" "Empathic" "Grappler" "Fighting Initiate" "Great Weapon Master"
   "Healer" "Heavy Armor Master" "Inspiring Leader" "Keen Mind" "Light Armor Master" "Magic Initiate"
   "Martial Scholar" "Medium Armor Master" "Metamagic Adept" "Mobile" "Mounted Combatant" "Polearm Master"
   "Resilient" "Ritual Caster" "Sentinel" "Sharpshooter" "Shield Master" "Skilled" "Skulker" "Specialist"
   "Spell Touched" "Summoner" "Survivor" "Tactician" "Telekinetic" "Telepathic" "War Caster" "Warlord"])

(defmethod randoms-factor :skills [_]
  5)
(defmethod randoms-preset :skills [{:keys [type]
                                    :or   {type :all}}]
  ["perception" "medicine" "deception" "persuasion" "investigation" "insight" "survival"
   "arcana" "athletics" "acrobatics" "sleight of hand" "stealth" "history"
   "nature" "religion" "animal handling" "intimidation" "performance" "engineering"])

(defmethod randoms-factor :damage-types [{:keys [type]
                                          :or   {type :all}}]
  (case type
    :physical 1
    :non-physical 2
    :all 3))
(defmethod randoms-preset :damage-types [{:keys [type]
                                          :or   {type :all}}]
  (case type
    :physical ["bludgeoning" "piercing" "slashing"]
    :non-physical ["acid" "cold" "fire" "force" "lightning" "necrotic" "poison" "psychic" "radiant" "thunder"]
    :all ["acid" "bludgeoning" "cold" "fire" "force" "lightning" "necrotic" "piercing" "poison" "psychic" "radiant" "slashing" "thunder"]))

(defmethod randoms-factor :ability-scores [{:keys [type]
                                            :or   {type :all}}]
  (case type
    :common 2
    :uncommon 2
    :all 4))
(defmethod randoms-preset :ability-scores [{:keys [type]
                                            :or   {type :all}}]
  (case type
    :common ["Constitution" "Dexterity" "Wisdom"]
    :uncommon ["Strength" "Intelligence" "Charisma"]
    :all ["Charisma" "Constitution" "Dexterity" "Intelligence" "Strength" "Wisdom"]))

(defmethod randoms-factor :monster-types [_] 1)
(defmethod randoms-preset :monster-types [_]
  ["Abberation" "Beast" "Celestial" "Construct" "Dragon" "Elemental" "Fey"
   "Fiend" "Giant" "Humanoid" "Monstrosity" "Ooze" "Plant" "Undead"])

(defmethod randoms-factor :cantrips [_] 2)
(defmethod randoms-preset :cantrips [_]
  ["Acid Splash" "Altered Strike" "Arcane Muscles" "Blade Ward" "Booming Blade" "Calculate" "Chill Touch"
   "Circular Breathing" "Control Flames" "Create Bonfire" "Dancing Lights" "Druidcraft" "Eldritch Blast"
   "Encode Thoughts" "Fire Bolt" "Friends" "Frostbite" "Grapevine" "Green-Flame Blade" "Guidance" "Gust"
   "Hypnic Jerk" "Infestation" "Light" "Lightning Lure" "Mage Hand" "Magic Stone" "Mending" "Message" "Mind Sliver"
   "Minor Illusion" "Mold Earth" "Pestilence" "Poison Spray" "Prestidigitation" "Primal Savagery" "Produce Flame"
   "Ray of Frost" "Resistance" "Sacred Flame" "Sapping Sting" "Shape Water" "Shillelagh" "Shocking Grasp"
   "Spare the Dying" "Sword Burst" "Thaumaturgy" "Thorn Whip" "Thunderclap" "Toll the Dead"
   "Tree Heal" "True Strike" "Vicious Mockery" "Word of Radiance"])

(defmethod randoms-factor :literal [{:keys [factor] :or {factor 1}}] factor)
(defmethod randoms-preset :literal [{:keys [values]}] values)

(defmethod randoms-factor :without-replacement [{:keys [amount from]}] (max amount (randoms-factor from)))
(defmethod randoms-preset :without-replacement [{:keys [amount from]}]
  (let [vs (random->values-vec from)]
    #(r/sample-without-replacement amount vs)))
