(ns campaign3.randoms
  (:require [randy.core :as r]))

(defmulti randoms-preset (comp keyword :preset))

(defn- keyword-type [{:keys [type] :as conf}]
  (cond-> conf
          (some? type) (update :type keyword)))

(defn- rand-from [vs]
  #(r/sample vs))

(def random->values-vec (comp randoms-preset keyword-type))

(defn randoms->fn [randoms]
  (cond
    (vector? randoms) (apply juxt (map (comp rand-from random->values-vec) randoms))
    (map? randoms) (randoms-preset randoms)))

(defn- random->weighting-multiplier [{:keys [preset] :as random}]
  (case preset
    :without-replacement (* (:amount random) (random->weighting-multiplier random))
    (count (random->values-vec random))))

(defn randoms->weighting-multiplier [randoms]
  (let [approx-num-options (cond
                             (vector? randoms) (transduce (map random->weighting-multiplier) + 0 randoms)
                             (map? randoms) (random->weighting-multiplier randoms)
                             (nil? randoms) 1)]
    (if (> approx-num-options 10)
      (->> (- approx-num-options 10)
           (double)
           (Math/sqrt)
           (int)
           (+ 10))
      approx-num-options)))

(defmethod randoms-preset :languages [_]
  ["Common" "Dwarvish" "Elvish" "Giant" "Gnomish" "Goblin" "Halfling" "Orc"
   "Abyssal" "Celestial" "Draconic" "Deep Speech" "Infernal" "Primordial" "Sylvan" "Undercommon"])

(defmethod randoms-preset :feats [_]
  ["Alert" "Athlete" "Actor" "Brawler" "Charger" "Chef" "Crossbow Expert" "Defensive Duelist"
   "Dual Wielder" "Dungeon Delver" "Durable" "Eldritch Adept" "Grappler" "Fighting Initiate"
   "Great Weapon Master" "Healer" "Heavy Armor Master" "Inspiring Leader" "Keen Mind" "Light Armor Master"
   "Mage Slayer" "Magic Initiate" "Martial Adept" "Medium Armor Master" "Metamagic Adept"
   "Mobile" "Mounted Combatant" "Polearm Master" "Resilient" "Ritual Caster" "Savage Attacker"
   "Sentinel" "Sharpshooter" "Shield Master" "Skilled" "Skulker" "Specialist" "Spell Sniper"
   "Spell Touched" "Telekinetic" "Telepathic" "War Caster"])

(defmethod randoms-preset :skills [{:keys [type]
                                    :or   {type :all}}]
  (case type
    :common ["perception" "medicine" "deception" "persuasion" "investigation" "insight" "survival"]
    :uncommon ["arcana" "athletics" "acrobatics" "sleight of hand" "stealth" "history"
               "nature" "religion" "animal handling" "intimidation" "performance"]
    :all ["perception" "medicine" "deception" "persuasion" "investigation" "insight" "survival"
          "arcana" "athletics" "acrobatics" "sleight of hand" "stealth" "history"
          "nature" "religion" "animal handling" "intimidation" "performance"])) ;TODO ensure none were missed

(defmethod randoms-preset :range [{:keys [args]}]
  (vec (apply range args)))

(defmethod randoms-preset :damage-types [{:keys [type]
                                          :or   {type :all}}]
  (case type
    :physical ["bludgeoning" "piercing" "slashing"]
    :non-physical ["acid" "cold" "fire" "force" "lightning" "necrotic" "poison" "psychic" "radiant" "thunder"]
    :elemental ["cold" "fire" "lightning"]
    :all ["acid" "bludgeoning" "cold" "fire" "force" "lightning" "necrotic" "piercing" "poison" "psychic" "radiant" "slashing" "thunder"]))

(defmethod randoms-preset :attributes [{:keys [type]
                                        :or   {type :all}}]
  (case type
    :common ["Constitution" "Dexterity" "Wisdom"]
    :uncommon ["Strength" "Intelligence" "Charisma"]
    :all ["Charisma" "Constitution" "Dexterity" "Intelligence" "Strength" "Wisdom"]))

(defmethod randoms-preset :monster-types [_]
  ["Abberation" "Beast" "Celestial" "Construct" "Dragon" "Elemental" "Fey"
   "Fiend" "Giant" "Humanoid" "Monstrosity" "Ooze" "Plant" "Undead"])

(defmethod randoms-preset :cantrips [_]
  ["Acid Splash" "Altered Strike" "Arcane Muscles" "Blade Ward" "Booming Blade" "Calculate" "Chill Touch"
   "Circular Breathing" "Control Flames" "Create Bonfire" "Dancing Lights" "Druidcraft" "Eldritch Blast"
   "Encode Thoughts" "Fire Bolt" "Friends" "Frostbite" "Grapevine" "Green-Flame Blade" "Guidance" "Gust"
   "Infestation" "Light" "Lightning Lure" "Mage Hand" "Magic Stone" "Mending" "Message" "Mind Sliver"
   "Minor Illusion" "Mold Earth" "Pestilence" "Poison Spray" "Prestidigitation" "Primal Savagery" "Produce Flame"
   "Ray of Frost" "Resistance" "Sacred Flame" "Sapping Sting" "Shape Water" "Shillelagh" "Shocking Grasp"
   "Spare the Dying" "Sword Burst" "Thaumaturgy" "Thorn Whip" "Thunderclap" "Toll the Dead" "True Strike"
   "Vicious Mockery" "Word of Radiance"])

(defmethod randoms-preset :without-replacement [{:keys [amount from]}]
  (let [vs (random->values-vec from)]
    #(r/sample-without-replacement amount vs)))
