(ns advent-2020.core
  (:require [clojure.string :as str])
  (:gen-class))

(def input-1
  (map read-string (str/split-lines (slurp "resources/input-1.txt"))))

(defn add-2-to-2020
  []
  (as-> input-1 <>
      (for [a <>] (map #(when (= 2020 (+ a %)) [a %]) <>))
      (flatten <>)
      (remove nil? <>)
      (set <>)
      (reduce * <>)))

(defn add-3-to-2020
  []
  (as-> input-1 <>
      (for [a <>] (for [b <>] (map #(when (= 2020 (+ a b %)) [a b %]) <>)))
      (flatten <>)
      (remove nil? <>)
      (set <>)
      (reduce * <>)))

(defn solve-day-1
  []
  (println "Solutions for Day 1:")
  (println "  A:" (add-2-to-2020))
  (println "  B:" (add-3-to-2020)))

(defn -main
  [d]
  (case (read-string d)
    1 (solve-day-1)))
