(ns story
  (:refer-clojure :exclude [assert]))

(defprotocol Story
  (assert [this statement])
  (consequent [this]))

(defprotocol Statement
  (refute [this story]) ;any objections? silence indicates none
  (fold [this story]))

(defn add [story statement]
  (assert story
    (if (clojure.test/function? statement)
      (statement story) ;yield to some indeterminate outcome
      statement)))
