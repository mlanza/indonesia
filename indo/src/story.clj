(ns story
  (:refer-clojure :exclude [assert]))

(defprotocol Story
  (assert [this statement])
  (consequent [this]))

(defprotocol Statement
  (refute [this story]) ;any objections? silence indicates none
  (fold [this story]))

(defn add [story f & args]
  (assert story (apply f args)))
