(ns story
  (:refer-clojure :exclude [assert]))

(defprotocol Story
  (assert [this statement])
  (consequent [this]))

(defprotocol Statement
  (refute [this story]) ;any objections? silence indicates none
  (fold [this story]))

(defprotocol Realize
  (realize [this story]))

(defn expand [this story]
  (if (satisfies? Realize this)
    (or (realize this story) this)
    this))
