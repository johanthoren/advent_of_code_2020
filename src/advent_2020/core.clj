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

(defn die
  "Print a `message` and exit the program with the given `status` code."
  [status message]
  (println message)
  (System/exit status))

(def input-2
  (str/split-lines (slurp "resources/input-2.txt")))

(def parsed-input-2
  (map #(remove empty? %) (map #(str/split % #"-|\s|:") input-2)))

(defn char-freq
  [c s]
  (get (frequencies s) (char (first c))))

(defn matches-in-toboggan-password
  [c p & positions]
  (for [x positions]
    (when (= c (subs p (dec x) x))
      true)))

(defn valid-sled-password?
  [min-c max-c c p]
  (when (re-find (re-pattern c) p)
    (let [char-count (char-freq c p)]
      (when (<= min-c char-count max-c)
        [min-c max-c c p]))))

(defn valid-toboggan-password?
  [pos-a pos-b c p]
  (let [m (matches-in-toboggan-password c p pos-a pos-b)
        t (remove nil? m)]
    (when (= 1 (count t))
      p)))

(defn all-valid-passwords
  [f]
  (remove nil? (for [i parsed-input-2]
                 (f (read-string (first i))
                    (read-string (second i))
                    (nth i 2)
                    (last i)))))

(defn solve-day-2
  []
  (println "Solutions for Day 2:")
  (println "  A:" (count (all-valid-passwords valid-sled-password?)))
  (println "  B:" (count (all-valid-passwords valid-toboggan-password?))))

(defn -main
  [d]
  (case (read-string d)
    1 (solve-day-1)
    2 (solve-day-2)
    (die 1 "Incorrect day provided.")))
