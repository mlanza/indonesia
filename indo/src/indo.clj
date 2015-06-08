(ns indo)

(defrecord Area [terrain spaces])
(defrecord Space [edges piece])
(defrecord Spot [name number])
(def spot ->Spot)

(defn area [terrain count]
  (->Area
    terrain
    (reduce
      (fn [m number]
        (assoc m
          (inc number)
          (->Space '() nil)))
     {}
     (range count))))

(defn locator [spot]
  #(get-in % [(:name spot) :spaces (:number spot)]))

(def provinces {
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
  "Maluku" 7
  "Musa Tenggara Barat" 2
  "Musa Tenggara Timur" 6
  "Papua" 9
  "Rian" 5
  "Sarawak" 4
  "Sulawesi Salatan" 3
  "Sulawesi Tengah" 5
  "Sulawesi Tenggara" 3
  "Sulawesi Utara" 2
  "Sumatera Barat" 4
  "Sumatera Selatan" 7
  "Sumatera Utara" 4})

(defn edge [spaces from & tos]
  (update-in spaces [(:name from) :spaces (:number from) :edges] #(concat % tos)))

(defn to-id
  ([name number]
    (clojure.string/lower-case (clojure.string/replace (str name " " number) " " "-")))
  ([spot]
   (to-id (:name spot) (:number spot))))

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
(def papua-8 (spot "Papua" 8))
(def papua-9 (spot "Papua" 9))
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

(def spaces
  (->
    (reduce-kv
      (fn [m k v]
        (assoc m k
          (area :land v)))
      {} provinces)
    (assoc "Ocean" (area :water 22))
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
    ))

(def spots
  (apply concat
    (map
      (fn [[name area]]
        (map
          (fn [number]
            (spot name number))
          (keys (:spaces area))))
      spaces)))

(defn def-pieces [spots]
  (sort
    (map
      #(str "(def " (to-id %) " " "(spot \"" (:name %) "\" " (:number %) "))")
    spots)))

(def aceh-1 (locator (spot "Aceh" 1)))

