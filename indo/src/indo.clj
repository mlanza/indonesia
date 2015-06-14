(ns indo)

;; TYPES

(defrecord Game [components players open-money phase turn-order era available-deeds actions])
(defrecord Player [color cash bank slots advancements city-cards])
(defrecord Components [board city-cards deeds city-limits]) ;allows for the possibility of other maps
(defrecord Board [areas edges spaces provinces pieces])
(defrecord City [size delivered-goods]) ;TODO delivered-goods an event-sourced calculation?
(defrecord CityCard [era area])
(defrecord Company [deeds])
(defrecord Deed [name piece era-maximums])
(defrecord Ship [company])
(defrecord Good [company])

;; CONSTRUCTORS

(def ship ->Ship)
(def good ->Good)
(def company ->Company)
(def components ->Components)

(defn board [areas edges]
  (->Board areas edges (apply clojure.set/union (vals areas)) (dissoc areas "Ocean") {}))

(defn player [color]
  (->Player color 100 0 #{} {:slots 1 :mergers 1 :hull 1 :expansion 1 :bid-multiplier 1} nil))

(defn city []
  (->City 1 nil))

(defn spot [name number]
  [name number])

(defn deed
  ([name piece]
    (deed name piece nil))
  ([name piece era-maximums]
    (->Deed name piece era-maximums)))

(declare consolidate)
(defn city-card [era & spots]
  (->CityCard era (consolidate spots)))

(declare deal-city-cards)
(defn game
  ([components players open-money]
    (->Game
      components
      (deal-city-cards (:city-cards components) players)
      open-money
      nil
      (shuffle (keys players))
      -1
      []
      []))
  ([components players]
    (game components players false)))

;; METADATA

(def bid-multiplier [1 5 25 100 400])
(def advancement [1 2 3 4 5])
(def phases [
  :new-era
  :bid-for-turn-order
  :mergers
  :aquisitions
  :research-and-development
  :operations
  :city-growth])

;; PURE FUNCTIONS

(defn consolidate [spots] ;equivalent of flatten for areas/spots
  (apply clojure.set/union
    (map
      (fn [spot]
        (if (set? spot) spot #{spot}))
      spots)))

(defn shuffle-city-cards [cards]
  (reduce-kv
    (fn [m era cards]
      (assoc m era (shuffle cards)))
    {}
   (group-by :era cards)))

(defn make-hands [cards]
  (partition 3
    (for [idx (range 0 5)
          era (range 0 3)]
      (get-in cards [era idx]))))

(defn reduce-indexed
  "Reduce while adding an index as the second argument to the function"
  ([f coll]
    (reduce-indexed f (first coll) 0 (rest coll)))
  ([f init coll]
    (reduce-indexed f init 0 coll))
  ([f init i coll]
    (if (empty? coll)
      init
      (let [v (first coll)
            fv (f init i v)]
        (recur f fv (inc i) (rest coll))))))

(defn update-players [f players]
  (reduce-indexed
    (fn [memo idx [player-name player]]
      (assoc memo player-name (f player idx)))
    {}
    players))

(defn deal-city-cards [cards players]
  (let [deal (comp vec make-hands shuffle-city-cards)
        hands (deal cards)
        f (if (= (count players) 2)
            (fn [player idx]
              (assoc player :city-cards (flatten (concat (get hands idx) (get hands (+ idx 2))))))
            (fn [player idx]
              (assoc player :city-cards (get hands idx))))]
    (update-players f players)))

(defn after [steps value]
  (if value
    (get steps (inc (.indexOf steps value)))
    (first steps)))

(defn around [steps value]
  (or (after steps value) (first steps)))

(defn step [steps value]
  (or (after steps value) (throw (Exception. "No room for advancement."))))

(defn advance [key]
  (or ({:bid-multiplier (partial step bid-multiplier)} key) (partial step advancement)))

(defn research-and-develop [player key]
  (update-in player [:advancements key] (advance key)))

(defn to-id
  ([name number]
    (clojure.string/lower-case (clojure.string/replace (str name " " number) " " "-")))
  ([[name number]]
   (to-id name number)))

(defn numbered [spot]
  (get spot 1))

(defn named [spot]
  (get spot 0))

(defn only [f area]
  (set
    (filter
      (fn [spot]
        (f (numbered spot)))
      area)))

(defn zone-area [name size]
  (consolidate (map #(spot name %) (range 1 (inc size)))))

(defn zone-areas [sizes]
  (reduce-kv
    (fn [m name size]
      (assoc m name (zone-area name size)))
    {}
    sizes))

(defn terrain [spot]
  (if (= "Ocean" (named spot)) :water :land))

(defn water? [spot]
  (= :water (terrain spot)))

(defn land? [spot]
  (not (water? spot)))

(def wet
  (partial filter water?))

(def dry
  (partial filter land?))

(defn sea-port? [board spot]
  (and (land? spot) (some water? (get-in board [:edges spot]))))

(defn inland? [board spot]
  (and (land? spot) (not-any? water? (get-in board [:edges spot]))))

(defn inland [board]
  (consolidate (filter (partial inland? board) (:spaces board))))

(defn edge [m from & tos]
  (assoc m from (consolidate tos)))

(defn adjacent [board area]
  (remove
    area
    (apply clojure.set/union
      (map (:edges board) area))))

(defn deed-types [deeds]
  (set (map #(get % :piece) (flatten deeds))))

(defn lacks-deeds? [deeds]
  (<= (count (deed-types deeds)) 1))

(defn capacity [spot]
  (if (land? spot) 1 999))

(defn pieces [board spot]
  (get-in board [:pieces spot]))

(defn room? [board spot]
  (< (count (pieces board spot)) (capacity spot)))

(defn room [board area]
  (set (filter (partial room? board) area)))

(defn seed-area [board deed] ;spots in which to seed the first piece of a company
  (let [area  ((:areas board) (:name deed))
        piece (:piece deed)
        adj   (partial adjacent board)]
    (if (= piece :ship)
      (-> area adj wet)
      area)))

(defn valid-deed? [board deed]
  (>
    (count
      (room board
        (seed-area
          board
          deed)))
    0))

(defn valid-deeds [board deeds]
  (filter (partial valid-deed? board) deeds))

(defn new-era? [game]
  (lacks-deeds? (get-in game [:available-deeds])))

(defn inc-era [game]
  (update-in game [:era] inc))

(defn deal-era-deeds [game]
  (let [era (:era game)
        deeds (get-in game [:components :deeds])
        new-deeds (get deeds era)]
    (update-in game [:available-deeds]
      (fn [available-deeds]
        (concat available-deeds new-deeds)))))

(defn update-cond [game pred f]
  (if (pred game) (f game) game))

(defn new-era [game]
  (-> game
    (update-in [:available-deeds] (partial valid-deeds (:board game))) ;eliminate invalid deeds
    (update-cond new-era? (comp deal-era-deeds inc-era))))

(defn has-city-card? [player city-card]
  (some #(= % city-card) (:city-cards player)))

(defn has-spot? [city-card spot]
  (some #(= % spot) (:area city-card)))

(def city?
  (partial instance? City))

(defn city-available? [size game]
  (let [city-limits (get-in game [:components :city-limits])
        limit (city-limits size)
        cities (filter
                 (fn [piece]
                   (and (city? piece) (= (:size piece) size)))
                 (get-in game [:components :board :pieces]))
        used (count cities)]
    (< used limit)))

(def starter-city-available?
  (partial city-available? 1))

(defn put-piece [piece spot game]
  (update-in game [:components :board :pieces spot]
    (fn [pieces]
      (cons piece pieces))))

(def not-nil?
  (complement nil?))

(defn valid-city-spot? [board spot]
  (and (sea-port? board spot) (room? board spot)))

(defn valid-city-spots [board area]
  (filter (partial valid-city-spot? board) area))

(defn validate-either [v f]
  (fn [& args]
    (let [message (apply v args)]
      (if message
        [message]
        [nil (apply f args)]))))

(defn action [key v f]
  (fn [game player-name & args]
    (let [message (apply v game player-name args)]
      (if message
        (throw (Exception. message))
        (update-in
          (apply f game player-name args)
          [:actions]
          conj
          (into [] (concat [key player-name] (vec args))))))))

(defn place-city? [game player-name city-card spot]
  (let [player  (get-in game [:players player-name])
        board   (get-in game [:components :board])
        starter (starter-city-available? game)
        pass    (nil? spot)
        play    (not pass)
        valid-spots (valid-city-spots board (:area city-card))]
    (cond
      (not (has-city-card? player city-card))       "Card not in hand."
      (and pass starter (not (empty? valid-spots))) "Cannot pass when a play is legal."
      (and play (not (has-spot? city-card spot)))   "Spot not on card."
      (and play (not (sea-port? board spot)))       "Not a sea port."
      (and play (not (room? board spot)))           "No room."
      (and play (not starter))                      "No starter cities available.")))

;TODO verify turn order of player actions
;TODO in general the args of functions should reflect what users would want to see in an action journal
(defn place-city* [game player-name city-card spot]
  (let [effect (if spot (partial put-piece (city) spot) identity)]
    (-> game
      effect
      (update-in [:players player-name :city-cards]
        (fn [city-cards] (remove (partial = city-card) city-cards))))))

(def place-city
  (action :place-city place-city? place-city*))

;; INDONESIA DATA

(def areas
  (zone-areas {
    "Aceh" 4
    "Bali" 2
    "Benakulu" 3
    "Halmahera" 6
    "Jambi" 3
    "Java Timur" 6
    "Java Tengah" 3
    "Jawa Barat" 7
    "Kalimatan Barat" 3
    "Kalimatan Salatan" 3
    "Kalimatan Tengah" 5
    "Kalimatan Timur" 5
    "Lampung" 4
    "Maluku" 9
    "Musa Tenggara Barat" 2
    "Musa Tenggara Timur" 6
    "Papua" 7
    "Rian" 5
    "Sarawak" 4
    "Sulawesi Salatan" 3
    "Sulawesi Tengah" 5
    "Sulawesi Tenggara" 3
    "Sulawesi Utara" 2
    "Sumatera Barat" 4
    "Sumatera Selatan" 7
    "Sumatera Utara" 4
    "Ocean" 22}))

(def aceh-1 (spot "Aceh" 1))
(def aceh-2 (spot "Aceh" 2))
(def aceh-3 (spot "Aceh" 3))
(def aceh-4 (spot "Aceh" 4))
(def bali-1 (spot "Bali" 1))
(def bali-2 (spot "Bali" 2))
(def benakulu-1 (spot "Benakulu" 1))
(def benakulu-2 (spot "Benakulu" 2))
(def benakulu-3 (spot "Benakulu" 3))
(def halmahera-1 (spot "Halmahera" 1))
(def halmahera-2 (spot "Halmahera" 2))
(def halmahera-3 (spot "Halmahera" 3))
(def halmahera-4 (spot "Halmahera" 4))
(def halmahera-5 (spot "Halmahera" 5))
(def halmahera-6 (spot "Halmahera" 6))
(def jambi-1 (spot "Jambi" 1))
(def jambi-2 (spot "Jambi" 2))
(def jambi-3 (spot "Jambi" 3))
(def java-tengah-1 (spot "Java Tengah" 1))
(def java-tengah-2 (spot "Java Tengah" 2))
(def java-tengah-3 (spot "Java Tengah" 3))
(def java-timur-1 (spot "Java Timur" 1))
(def java-timur-2 (spot "Java Timur" 2))
(def java-timur-3 (spot "Java Timur" 3))
(def java-timur-4 (spot "Java Timur" 4))
(def java-timur-5 (spot "Java Timur" 5))
(def java-timur-6 (spot "Java Timur" 6))
(def jawa-barat-1 (spot "Jawa Barat" 1))
(def jawa-barat-2 (spot "Jawa Barat" 2))
(def jawa-barat-3 (spot "Jawa Barat" 3))
(def jawa-barat-4 (spot "Jawa Barat" 4))
(def jawa-barat-5 (spot "Jawa Barat" 5))
(def jawa-barat-6 (spot "Jawa Barat" 6))
(def jawa-barat-7 (spot "Jawa Barat" 7))
(def kalimatan-barat-1 (spot "Kalimatan Barat" 1))
(def kalimatan-barat-2 (spot "Kalimatan Barat" 2))
(def kalimatan-barat-3 (spot "Kalimatan Barat" 3))
(def kalimatan-salatan-1 (spot "Kalimatan Salatan" 1))
(def kalimatan-salatan-2 (spot "Kalimatan Salatan" 2))
(def kalimatan-salatan-3 (spot "Kalimatan Salatan" 3))
(def kalimatan-tengah-1 (spot "Kalimatan Tengah" 1))
(def kalimatan-tengah-2 (spot "Kalimatan Tengah" 2))
(def kalimatan-tengah-3 (spot "Kalimatan Tengah" 3))
(def kalimatan-tengah-4 (spot "Kalimatan Tengah" 4))
(def kalimatan-tengah-5 (spot "Kalimatan Tengah" 5))
(def kalimatan-timur-1 (spot "Kalimatan Timur" 1))
(def kalimatan-timur-2 (spot "Kalimatan Timur" 2))
(def kalimatan-timur-3 (spot "Kalimatan Timur" 3))
(def kalimatan-timur-4 (spot "Kalimatan Timur" 4))
(def kalimatan-timur-5 (spot "Kalimatan Timur" 5))
(def lampung-1 (spot "Lampung" 1))
(def lampung-2 (spot "Lampung" 2))
(def lampung-3 (spot "Lampung" 3))
(def lampung-4 (spot "Lampung" 4))
(def maluku-1 (spot "Maluku" 1))
(def maluku-2 (spot "Maluku" 2))
(def maluku-3 (spot "Maluku" 3))
(def maluku-4 (spot "Maluku" 4))
(def maluku-5 (spot "Maluku" 5))
(def maluku-6 (spot "Maluku" 6))
(def maluku-7 (spot "Maluku" 7))
(def maluku-8 (spot "Maluku" 8))
(def maluku-9 (spot "Maluku" 9))
(def musa-tenggara-barat-1 (spot "Musa Tenggara Barat" 1))
(def musa-tenggara-barat-2 (spot "Musa Tenggara Barat" 2))
(def musa-tenggara-timur-1 (spot "Musa Tenggara Timur" 1))
(def musa-tenggara-timur-2 (spot "Musa Tenggara Timur" 2))
(def musa-tenggara-timur-3 (spot "Musa Tenggara Timur" 3))
(def musa-tenggara-timur-4 (spot "Musa Tenggara Timur" 4))
(def musa-tenggara-timur-5 (spot "Musa Tenggara Timur" 5))
(def musa-tenggara-timur-6 (spot "Musa Tenggara Timur" 6))
(def papua-1 (spot "Papua" 1))
(def papua-2 (spot "Papua" 2))
(def papua-3 (spot "Papua" 3))
(def papua-4 (spot "Papua" 4))
(def papua-5 (spot "Papua" 5))
(def papua-6 (spot "Papua" 6))
(def papua-7 (spot "Papua" 7))
(def rian-1 (spot "Rian" 1))
(def rian-2 (spot "Rian" 2))
(def rian-3 (spot "Rian" 3))
(def rian-4 (spot "Rian" 4))
(def rian-5 (spot "Rian" 5))
(def sarawak-1 (spot "Sarawak" 1))
(def sarawak-2 (spot "Sarawak" 2))
(def sarawak-3 (spot "Sarawak" 3))
(def sarawak-4 (spot "Sarawak" 4))
(def sulawesi-salatan-1 (spot "Sulawesi Salatan" 1))
(def sulawesi-salatan-2 (spot "Sulawesi Salatan" 2))
(def sulawesi-salatan-3 (spot "Sulawesi Salatan" 3))
(def sulawesi-tengah-1 (spot "Sulawesi Tengah" 1))
(def sulawesi-tengah-2 (spot "Sulawesi Tengah" 2))
(def sulawesi-tengah-3 (spot "Sulawesi Tengah" 3))
(def sulawesi-tengah-4 (spot "Sulawesi Tengah" 4))
(def sulawesi-tengah-5 (spot "Sulawesi Tengah" 5))
(def sulawesi-tenggara-1 (spot "Sulawesi Tenggara" 1))
(def sulawesi-tenggara-2 (spot "Sulawesi Tenggara" 2))
(def sulawesi-tenggara-3 (spot "Sulawesi Tenggara" 3))
(def sulawesi-utara-1 (spot "Sulawesi Utara" 1))
(def sulawesi-utara-2 (spot "Sulawesi Utara" 2))
(def sumatera-barat-1 (spot "Sumatera Barat" 1))
(def sumatera-barat-2 (spot "Sumatera Barat" 2))
(def sumatera-barat-3 (spot "Sumatera Barat" 3))
(def sumatera-barat-4 (spot "Sumatera Barat" 4))
(def sumatera-selatan-1 (spot "Sumatera Selatan" 1))
(def sumatera-selatan-2 (spot "Sumatera Selatan" 2))
(def sumatera-selatan-3 (spot "Sumatera Selatan" 3))
(def sumatera-selatan-4 (spot "Sumatera Selatan" 4))
(def sumatera-selatan-5 (spot "Sumatera Selatan" 5))
(def sumatera-selatan-6 (spot "Sumatera Selatan" 6))
(def sumatera-selatan-7 (spot "Sumatera Selatan" 7))
(def sumatera-utara-1 (spot "Sumatera Utara" 1))
(def sumatera-utara-2 (spot "Sumatera Utara" 2))
(def sumatera-utara-3 (spot "Sumatera Utara" 3))
(def sumatera-utara-4 (spot "Sumatera Utara" 4))
(def ocean-1  (spot "Ocean" 1))
(def ocean-2  (spot "Ocean" 2))
(def ocean-3  (spot "Ocean" 3))
(def ocean-4  (spot "Ocean" 4))
(def ocean-5  (spot "Ocean" 5))
(def ocean-6  (spot "Ocean" 6))
(def ocean-7  (spot "Ocean" 7))
(def ocean-8  (spot "Ocean" 8))
(def ocean-9  (spot "Ocean" 9))
(def ocean-10 (spot "Ocean" 10))
(def ocean-11 (spot "Ocean" 11))
(def ocean-12 (spot "Ocean" 12))
(def ocean-13 (spot "Ocean" 13))
(def ocean-14 (spot "Ocean" 14))
(def ocean-15 (spot "Ocean" 15))
(def ocean-16 (spot "Ocean" 16))
(def ocean-17 (spot "Ocean" 17))
(def ocean-18 (spot "Ocean" 18))
(def ocean-19 (spot "Ocean" 19))
(def ocean-20 (spot "Ocean" 20))
(def ocean-21 (spot "Ocean" 21))
(def ocean-22 (spot "Ocean" 22))

(def aceh (areas "Aceh"))
(def bali (areas "Bali"))
(def benakulu (areas "Benakulu"))
(def halmahera (areas "Halmahera"))
(def jambi (areas "Jambi"))
(def java-tengah (areas "Java Tengah"))
(def java-timur (areas "Java Timur"))
(def jawa-barat (areas "Jawa Barat"))
(def kalimatan-barat (areas "Kalimatan Barat"))
(def kalimatan-salatan (areas "Kalimatan Salatan"))
(def kalimatan-tengah (areas "Kalimatan Tengah"))
(def kalimatan-timur (areas "Kalimatan Timur"))
(def lampung (areas "Lampung"))
(def maluku (areas "Maluku"))
(def musa-tenggara-barat (areas "Musa Tenggara Barat"))
(def musa-tenggara-timur (areas "Musa Tenggara Timur"))
(def papua (areas "Papua"))
(def rian (areas "Rian"))
(def sarawak (areas "Sarawak"))
(def sulawesi-salatan (areas "Sulawesi Salatan"))
(def sulawesi-tengah (areas "Sulawesi Tengah"))
(def sulawesi-tenggara (areas "Sulawesi Tenggara"))
(def sulawesi-utara (areas "Sulawesi Utara"))
(def sumatera-barat (areas "Sumatera Barat"))
(def sumatera-selatan (areas "Sumatera Selatan"))
(def sumatera-utara (areas "Sumatera Utara"))
(def ocean (areas "Ocean"))

(def edges
  (-> {}
    (edge aceh-1 aceh-2 aceh-3 ocean-12)
    (edge aceh-2 ocean-12 ocean-14 sumatera-utara-1 aceh-3 aceh-1)
    (edge aceh-3 ocean-12 aceh-1 aceh-2 sumatera-utara-1 sumatera-utara-3 aceh-4)
    (edge sumatera-utara-1 aceh-3 aceh-2 ocean-14 sumatera-utara-2 sumatera-utara-3)
    (edge sumatera-utara-2 ocean-14 rian-1 rian-5 sumatera-utara-3 sumatera-utara-1)
    (edge sumatera-utara-3 ocean-11 sumatera-utara-4 ocean-12 aceh-3 sumatera-utara-1 sumatera-utara-2 rian-5 sumatera-barat-1)
    (edge sumatera-utara-4 ocean-11 aceh-4 sumatera-barat-4)
    (edge sumatera-barat-1 sumatera-utara-3 rian-5 rian-4 sumatera-barat-2 ocean-11)
    (edge sumatera-barat-2 rian-4 jambi-1 benakulu-1 ocean-11 sumatera-barat-3 sumatera-barat-1)
    (edge sumatera-barat-3 ocean-11 sumatera-barat-4 sumatera-barat-2)
    (edge sumatera-barat-4 ocean-11 sumatera-utara-4 sumatera-barat-3)
    (edge rian-1 ocean-14 ocean-15 rian-3 rian-4 rian-5 sumatera-utara-2)
    (edge rian-2 ocean-15 rian-3)
    (edge rian-3 ocean-15 rian-2 jambi-2 jambi-1 sumatera-barat-2 rian-4 rian-1)
    (edge rian-4 sumatera-barat-2 sumatera-barat-1 rian-5 rian-1 rian-3)
    (edge rian-5 rian-1 rian-4 sumatera-barat-1 sumatera-utara-3 sumatera-utara-2)
    (edge jambi-1 jambi-2 sumatera-selatan-7 benakulu-2 benakulu-1 sumatera-barat-2 rian-4 rian-3)
    (edge jambi-2 rian-3 ocean-15 jambi-3 sumatera-selatan-3 sumatera-selatan-7 jambi-1)
    (edge jambi-3 ocean-15 ocean-16 sumatera-selatan-3 jambi-2)
    (edge benakulu-1 sumatera-barat-2 jambi-1 benakulu-2 ocean-11)
    (edge benakulu-2 jambi-1 sumatera-selatan-7 sumatera-selatan-6 benakulu-3 ocean-11 benakulu-1)
    (edge benakulu-3 sumatera-selatan-6 lampung-3 ocean-10 ocean-11 benakulu-2)
    (edge sumatera-selatan-1 ocean-18 sumatera-selatan-2)
    (edge sumatera-selatan-2 ocean-16 sumatera-selatan-4 sumatera-selatan-1)
    (edge sumatera-selatan-3 jambi-3 ocean-16 sumatera-selatan-4 sumatera-selatan-5 sumatera-selatan-7 jambi-2)
    (edge sumatera-selatan-4 ocean-16 sumatera-selatan-2 lampung-1 lampung-4 sumatera-selatan-5 sumatera-selatan-3)
    (edge sumatera-selatan-5 lampung-4 lampung-3 sumatera-selatan-6 sumatera-selatan-7 sumatera-selatan-3 sumatera-selatan-4)
    (edge sumatera-selatan-6 lampung-3 benakulu-3 benakulu-2 sumatera-selatan-7 sumatera-selatan-5)
    (edge sumatera-selatan-7 jambi-1 jambi-2 sumatera-selatan-3 sumatera-selatan-5 sumatera-selatan-6 benakulu-2)
    (edge lampung-1 ocean-16 ocean-17 lampung-2 lampung-4 sumatera-selatan-4)
    (edge lampung-2 ocean-17 ocean-10 lampung-3 lampung-4 lampung-1)
    (edge lampung-3 benakulu-3 sumatera-selatan-6 lampung-4 lampung-2 ocean-10)
    (edge lampung-4 sumatera-selatan-5 sumatera-selatan-4 lampung-1 lampung-2 lampung-3)
    (edge jawa-barat-1 ocean-17 jawa-barat-2 jawa-barat-7 ocean-10)
    (edge jawa-barat-2 ocean-17 jawa-barat-3 jawa-barat-7 jawa-barat-1)
    (edge jawa-barat-3 jawa-barat-4 jawa-barat-6 jawa-barat-7 jawa-barat-2 ocean-17)
    (edge jawa-barat-4 ocean-17 java-tengah-1 jawa-barat-5 jawa-barat-6 jawa-barat-3)
    (edge jawa-barat-5 ocean-9 ocean-10 jawa-barat-6 jawa-barat-4 java-tengah-1)
    (edge jawa-barat-6 ocean-10 jawa-barat-7 jawa-barat-3 jawa-barat-4 jawa-barat-5)
    (edge jawa-barat-7 ocean-10 jawa-barat-1 jawa-barat-2 jawa-barat-3 jawa-barat-6)
    (edge java-tengah-1 ocean-17 ocean-21 java-tengah-2 java-tengah-3 ocean-9 jawa-barat-5 jawa-barat-4)
    (edge java-tengah-2 ocean-21 java-timur-1 java-timur-6 java-tengah-3 java-tengah-1)
    (edge java-tengah-3 java-timur-6 ocean-9 java-tengah-1 java-tengah-2)
    (edge java-timur-1 ocean-21 java-timur-2 java-timur-5 java-timur-6 java-tengah-2)
    (edge java-timur-2 ocean-21 java-timur-1)
    (edge java-timur-3 bali-2 ocean-9 java-timur-4 ocean-21)
    (edge java-timur-4 ocean-21 java-timur-3 ocean-9 java-timur-5)
    (edge java-timur-5 ocean-21 java-timur-4 ocean-9 java-timur-6 java-timur-1)
    (edge java-timur-6 ocean-9 java-tengah-3 java-tengah-2 java-timur-1 java-timur-5)
    (edge bali-1 bali-2 musa-tenggara-barat-2 ocean-8)
    (edge bali-2 bali-1 ocean-8 java-timur-3)
    (edge musa-tenggara-barat-1 musa-tenggara-timur-2 musa-tenggara-barat-2 ocean-8)
    (edge musa-tenggara-barat-2 musa-tenggara-barat-1 bali-1 ocean-8)
    (edge musa-tenggara-timur-1 ocean-8 musa-tenggara-timur-2)
    (edge musa-tenggara-timur-2 musa-tenggara-timur-3 musa-tenggara-timur-1 musa-tenggara-barat-1 ocean-7)
    (edge musa-tenggara-timur-3 musa-tenggara-timur-2 musa-tenggara-timur-5 ocean-7)
    (edge musa-tenggara-timur-4 musa-tenggara-timur-5 ocean-7)
    (edge musa-tenggara-timur-5 maluku-1 musa-tenggara-timur-4 musa-tenggara-timur-3 musa-tenggara-timur-6 ocean-7)
    (edge musa-tenggara-timur-6 musa-tenggara-timur-5 ocean-7)
    (edge maluku-1 musa-tenggara-timur-5 maluku-8 ocean-5)
    (edge maluku-2 maluku-3 ocean-5)
    (edge maluku-3 maluku-2 maluku-4 ocean-5)
    (edge maluku-4 maluku-3 maluku-5 ocean-5)
    (edge maluku-5 maluku-6 maluku-4 ocean-5)
    (edge maluku-6 maluku-7 maluku-5 ocean-2)
    (edge maluku-7 maluku-6 ocean-2)
    (edge maluku-8 maluku-1 maluku-9 ocean-4)
    (edge maluku-9 papua-3 maluku-8 ocean-4)
    (edge papua-1 ocean-3 papua-2)
    (edge papua-2 ocean-3 papua-1 papua-6 papua-7)
    (edge papua-3 papua-5 papua-4 ocean-4 maluku-9 papua-7)
    (edge papua-4 ocean-4 papua-3 papua-5)
    (edge papua-5 papua-4 papua-3 papua-7 ocean-3)
    (edge papua-6 maluku-1 ocean-3)
    (edge papua-7 maluku-1 ocean-3 papua-5 papua-3 ocean-4)
    (edge halmahera-1 halmahera-3 halmahera-2 ocean-2)
    (edge halmahera-2 halmahera-3 halmahera-1 ocean-2)
    (edge halmahera-3 halmahera-1 halmahera-2 halmahera-5 halmahera-4 ocean-2)
    (edge halmahera-4 halmahera-3 ocean-2)
    (edge halmahera-5 halmahera-3 halmahera-6 ocean-2)
    (edge halmahera-6 halmahera-5 ocean-2)
    (edge sulawesi-tenggara-1 ocean-6 sulawesi-tenggara-2)
    (edge sulawesi-tenggara-2 sulawesi-tenggara-1 sulawesi-tenggara-3 ocean-6)
    (edge sulawesi-tenggara-3 sulawesi-salatan-2 sulawesi-tengah-3 ocean-20 sulawesi-tenggara-2 ocean-6)
    (edge sulawesi-salatan-1 sulawesi-tengah-2 sulawesi-salatan-2 ocean-1)
    (edge sulawesi-salatan-2 sulawesi-salatan-3 ocean-6 ocean-1 sulawesi-salatan-1)
    (edge sulawesi-salatan-3 ocean-6 sulawesi-salatan-2)
    (edge sulawesi-tengah-1 sulawesi-utara-2 ocean-20 sulawesi-tengah-2 ocean-1)
    (edge sulawesi-tengah-2 sulawesi-tengah-3 sulawesi-salatan-2 sulawesi-salatan-1 sulawesi-tengah-1 ocean-20 ocean-1)
    (edge sulawesi-tengah-3 sulawesi-tenggara-3 sulawesi-salatan-2 sulawesi-tengah-2 ocean-20 sulawesi-tengah-5)
    (edge sulawesi-tengah-4 ocean-20 sulawesi-tengah-3)
    (edge sulawesi-tengah-5 sulawesi-utara-2 sulawesi-tengah-3 ocean-20)
    (edge sulawesi-utara-1 ocean-20 sulawesi-utara-2)
    (edge sulawesi-utara-2 sulawesi-utara-1 sulawesi-tengah-5 sulawesi-tengah-1 ocean-20 ocean-1)
    (edge kalimatan-salatan-1 kalimatan-salatan-3 kalimatan-salatan-2 ocean-19)
    (edge kalimatan-salatan-2 kalimatan-salatan-1 kalimatan-salatan-3 kalimatan-tengah-1 kalimatan-timur-1 ocean-19)
    (edge kalimatan-salatan-3 kalimatan-tengah-1 kalimatan-salatan-2 kalimatan-salatan-1 ocean-19)
    (edge kalimatan-barat-1 sarawak-4 kalimatan-timur-5 kalimatan-tengah-2 kalimatan-barat-2)
    (edge kalimatan-barat-2 sarawak-4 kalimatan-barat-1 kalimatan-tengah-3 ocean-18 kalimatan-barat-3)
    (edge kalimatan-barat-3 ocean-13 ocean-18 kalimatan-barat-2 sarawak-4)
    (edge kalimatan-timur-1 ocean-19 kalimatan-salatan-2 kalimatan-tengah-1 kalimatan-timur-2 ocean-1)
    (edge kalimatan-timur-2 ocean-1 kalimatan-timur-1 kalimatan-tengah-1 kalimatan-tengah-2 kalimatan-timur-5 kalimatan-timur-3)
    (edge kalimatan-timur-3 kalimatan-timur-2 kalimatan-timur-5 kalimatan-timur-4 ocean-1)
    (edge kalimatan-timur-4 ocean-1 kalimatan-timur-3 kalimatan-timur-5 sarawak-3 sarawak-1)
    (edge kalimatan-timur-5 kalimatan-timur-4 kalimatan-timur-3 kalimatan-timur-2 kalimatan-tengah-2 kalimatan-barat-1 sarawak-4 sarawak-3)
    (edge kalimatan-tengah-1 kalimatan-salatan-3 kalimatan-salatan-2 kalimatan-timur-1 kalimatan-timur-2 kalimatan-tengah-2 kalimatan-tengah-5 ocean-19)
    (edge kalimatan-tengah-2 kalimatan-timur-2 kalimatan-tengah-1 kalimatan-tengah-5 kalimatan-tengah-4 kalimatan-tengah-3 kalimatan-barat-1 kalimatan-timur-5)
    (edge kalimatan-tengah-3 kalimatan-barat-2 kalimatan-tengah-2 kalimatan-tengah-4 ocean-18)
    (edge kalimatan-tengah-4 kalimatan-tengah-3 kalimatan-tengah-2 kalimatan-tengah-5 ocean-19 ocean-18)
    (edge kalimatan-tengah-5 kalimatan-tengah-1 kalimatan-tengah-2 kalimatan-tengah-4 ocean-19)
    (edge sarawak-1 ocean-1 kalimatan-timur-4 sarawak-3 sarawak-2)
    (edge sarawak-2 ocean-13 sarawak-1 sarawak-3)
    (edge sarawak-3 sarawak-2 sarawak-1 kalimatan-timur-4 kalimatan-timur-5 sarawak-4 ocean-13)
    (edge sarawak-4 sarawak-3 kalimatan-timur-5 kalimatan-barat-1 kalimatan-barat-2 kalimatan-barat-3 ocean-13)
    (edge ocean-1 ocean-2 ocean-20 ocean-6 ocean-19 ocean-13 sulawesi-utara-2 sulawesi-tengah-1 sulawesi-tengah-2 sulawesi-salatan-1 sulawesi-salatan-2 kalimatan-timur-1 kalimatan-timur-2 kalimatan-timur-3 kalimatan-timur-4 sarawak-1)
    (edge ocean-2 ocean-3 ocean-5 ocean-6 ocean-20 ocean-1 maluku-7 maluku-6 halmahera-1 halmahera-2 halmahera-3 halmahera-4 halmahera-5 halmahera-6)
    (edge ocean-3 papua-1 papua-2 papua-6 papua-5 papua-7 ocean-2 ocean-5 ocean-4)
    (edge ocean-4 ocean-5 ocean-3 maluku-8 maluku-9 papua-7 papua-3 papua-4)
    (edge ocean-5 ocean-7 ocean-6 ocean-2 ocean-3 ocean-4 maluku-1 maluku-5 maluku-4 maluku-3 maluku-2)
    (edge ocean-6 ocean-20 ocean-2 ocean-5 ocean-7 ocean-8 ocean-19 ocean-1 sulawesi-salatan-3 sulawesi-salatan-2 sulawesi-tenggara-3 sulawesi-tenggara-2 sulawesi-tenggara-1)
    (edge ocean-7 ocean-8 ocean-6 ocean-5 musa-tenggara-timur-2 musa-tenggara-timur-3 musa-tenggara-timur-4 musa-tenggara-timur-5 musa-tenggara-timur-6)
    (edge ocean-8 ocean-9 ocean-21 ocean-19 ocean-6 ocean-7 bali-2 bali-1 musa-tenggara-barat-2 musa-tenggara-barat-1 musa-tenggara-timur-1)
    (edge ocean-9 ocean-8 ocean-10 jawa-barat-5 java-tengah-1 java-tengah-3 java-timur-6 java-timur-5 java-timur-4 java-timur-3)
    (edge ocean-10 ocean-17 ocean-9 ocean-22 ocean-11 benakulu-3 lampung-3 lampung-2 jawa-barat-1 jawa-barat-7 jawa-barat-6 jawa-barat-5)
    (edge ocean-11 ocean-10 ocean-22 ocean-12 sumatera-barat-3 sumatera-barat-4 sumatera-utara-4 sumatera-utara-3 sumatera-barat-1 sumatera-barat-2 benakulu-1 benakulu-2 benakulu-3)
    (edge ocean-12 ocean-13 ocean-14 ocean-11 ocean-22 aceh-4 aceh-3 aceh-1 aceh-2)
    (edge ocean-13 ocean-12 ocean-14 ocean-15 ocean-18 ocean-1 sarawak-2 sarawak-3 sarawak-4 kalimatan-barat-3)
    (edge ocean-14 ocean-12 ocean-13 ocean-15 rian-1 sumatera-utara-2 sumatera-utara-1 aceh-2)
    (edge ocean-15 ocean-14 ocean-13 ocean-18 ocean-16 rian-2 rian-1 rian-3 jambi-2 jambi-3)
    (edge ocean-16 ocean-15 ocean-18 ocean-17 lampung-1 sumatera-selatan-4 sumatera-selatan-3 sumatera-selatan-2 jambi-3)
    (edge ocean-17 ocean-16 ocean-18 ocean-21 ocean-10 java-tengah-1 jawa-barat-4 jawa-barat-3 jawa-barat-2 jawa-barat-1 lampung-1 lampung-2)
    (edge ocean-18 ocean-19 ocean-21 ocean-17 ocean-16 ocean-15 ocean-13 kalimatan-barat-3 kalimatan-barat-2 kalimatan-tengah-3 sumatera-selatan-1)
    (edge ocean-19 ocean-1 ocean-6 ocean-8 ocean-21 ocean-18 kalimatan-tengah-4 kalimatan-tengah-5 kalimatan-tengah-1 kalimatan-salatan-3 kalimatan-salatan-1 kalimatan-salatan-2 kalimatan-timur-1)
    (edge ocean-20 ocean-1 ocean-2 ocean-6 sulawesi-tenggara-2 sulawesi-tenggara-3 sulawesi-tengah-3 sulawesi-tengah-4 sulawesi-tengah-5 sulawesi-tengah-2 sulawesi-tengah-1 sulawesi-utara-2 sulawesi-utara-1)
    (edge ocean-21 ocean-17 ocean-18 ocean-19 ocean-8 java-timur-3 java-timur-4 java-timur-5 java-timur-1 java-timur-2 java-tengah-2 java-tengah-1)
    (edge ocean-22 ocean-12 ocean-11 ocean-10)))

(def city-cards [
  (city-card 0 sulawesi-salatan java-timur sumatera-selatan)
  (city-card 0 sulawesi-utara sulawesi-salatan jawa-barat)
  (city-card 0 sulawesi-utara bali java-tengah)
  (city-card 0 java-timur jawa-barat bali)
  (city-card 0 sumatera-selatan jawa-barat java-tengah)
  (city-card 1 (only #{1 2 3} aceh) lampung maluku)
  (city-card 1 (only #{1 2 3} aceh) (only #{1 2 3} sumatera-utara) benakulu)
  (city-card 1 (only #{1 2 3} sumatera-utara) kalimatan-salatan (only #{1 2 3 4 5 8 9} maluku))
  (city-card 1 (only #{1 2} sumatera-barat) lampung kalimatan-salatan)
  (city-card 1 (only #{1 2} sumatera-barat) benakulu jawa-barat)
  (city-card 2 halmahera papua musa-tenggara-barat)
  (city-card 2 halmahera (only #{2 3 4 5 6} musa-tenggara-timur) jawa-barat)
  (city-card 2 sarawak sulawesi-tengah papua)
  (city-card 2 sarawak musa-tenggara-barat jambi)
  (city-card 2 jambi sulawesi-tengah (only #{2 3 4 5 6} musa-tenggara-timur))])

(def deeds [ ;grouped by era
 [(deed "Halmahera" :spice)
  (deed "Maluku" :spice)
  (deed "Jawa Barat" :rice)
  (deed "Java Timur" :ship [2 3 3])
  (deed "Lampung" :ship [2 3 4])
  (deed "Sulawesi Salatan" :ship [3 3 4])
  (deed "Halmahera" :ship [3 4 5])
  (deed "Bali" :rice)]
 [(deed "Aceh" :rice)
  (deed "Kalimatan Timur" :rice)
  (deed "Sulawesi Tengah" :spice)
  (deed "Java Tengah" :spice)
  (deed "Rian" :rubber)
  (deed "Sumatera Barat" :rubber)
  (deed "Kalimatan Barat" :rubber)
  (deed "Jawa Barat" :ship [0 4 5])
  (deed "Sumatera Utara" :ship [0 4 5])]
 [(deed "Bali" :rice)
  (deed "Sulawesi Tenggara" :rice)
  (deed "Papua" :oil)
  (deed "Kalimatan Salatan" :oil)
  (deed "Sumatera Selatan" :spice)
  (deed "Sarawak" :oil)
  (deed "Papua" :rubber)
  (deed "Maluku" :oil)]])

(def indonesia
  (components (board areas edges) city-cards deeds {1 12 2 8 3 3}))
