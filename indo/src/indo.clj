(ns indo)

(defprotocol Story
  (state [this statement])
  (anticipate [this]))

(defprotocol Statement
  (balk [this story])) ;any objections?

(defrecord Session [components statements]
  Story
  (state [this statement]
    (let [error (balk statement this)]
      (if error
        (throw (Exception. error))
        (update-in this [:statements] conj statement))))
  (anticipate [this])) ;(last (:statements this))

(def empty-session
  (->Session nil []))

(defrecord Seat [players open-cash]
  Statement
  (balk [this session]))
(defn seat
  ([players open-cash]
    (->Seat players open-cash))
  ([players]
    (seat players false)))

(defrecord Deal [hands]
  Statement
  (balk [this session]))
(def deal ->Deal)

(defrecord TurnOrder [players]
  Statement
  (balk [this session]))
(def turn-order ->TurnOrder)

(defrecord Era [label]
  Statement
  (balk [this session]))
(def era ->Era)

(defn hand [& cards]
  (into #{} cards))

(def session
  (-> empty-session
    (state (seat {"Mario" "white" "Rick" "black" "Sean" "red" "Steve" "green"}))
    (state (deal {"Mario" (hand) "Rick" (hand) "Sean" (hand) "Steve" (hand)}))
    (state (turn-order ["Rick" "Sean" "Steve" "Mario"]))
    (state (era \A))
    ))
