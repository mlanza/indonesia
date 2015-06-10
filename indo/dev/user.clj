(ns user
  "Tools for interactive development with the REPL. This file should not be included in a production build of the application."
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [indo]))

(refer 'indo)

(defn sample []
  (init [(player "Mario" :white) (player "Rick" :black) (player "Sean" :green) (player "Steve" :yellow)]))

(def system (atom nil))

(defn start []
  (reset! system (sample)))

(start)
