(ns story
  (:refer-clojure :exclude [assert resolve]))

(defprotocol Story
  (assert [this statement])
  (consequent [this]))

(defprotocol Statement
  (refute [this story]) ;any objections? silence indicates none
  (fold [this story]))

(defprotocol Chance
  (resolve [this story]))

(defn ordain [this story]
  (if (satisfies? Chance this)
    (or (resolve this story) this)
    this))
