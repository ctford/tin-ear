(ns sn.melody
  (:require
    [leipzig.melody :refer :all]))

(defn vary [f notes]
  (->> notes (then (f notes))))

(defn part [p notes]
  (->> notes (all :part p)))
