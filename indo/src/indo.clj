(ns indo
  (:refer-clojure :exclude [assert]))

(defprotocol Story
  (assert [this statement])
  (anticipate [this]))

(defprotocol Statement
  (refute [this story])) ;any objections?

(defrecord Session [components statements]
  Story
  (assert [this statement]
    (let [error (refute statement this)]
      (if error
        (throw (Exception. error))
        (update-in this [:statements] conj statement))))
  (anticipate [this])) ;(last (:statements this))

(def empty-session
  (->Session nil []))

(defrecord Seat [players open-cash]
  Statement
  (refute [this session]))
(defn seat
  ([players open-cash]
    (->Seat players open-cash))
  ([players]
    (seat players false)))

(defrecord Deal [hands]
  Statement
  (refute [this session]))
(def deal ->Deal)

(defrecord TurnOrder [players]
  Statement
  (refute [this session]))
(def turn-order ->TurnOrder)

(defrecord Era [label]
  Statement
  (refute [this session]))
(def era ->Era)

(defn hand [& cards]
  (into #{} cards))

(def session
  (-> empty-session
    (assert (seat {"Mario" "white" "Rick" "black" "Sean" "red" "Steve" "green"}))
    (assert (deal {"Mario" (hand) "Rick" (hand) "Sean" (hand) "Steve" (hand)}))
    (assert (turn-order ["Rick" "Sean" "Steve" "Mario"]))
    (assert (era \A))
    ))
