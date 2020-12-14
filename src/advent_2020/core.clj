(ns advent-2020.core
  (:require [clojure.set :refer [difference intersection]]
            [clojure.string :as str])
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

(def input-3
  (str/split-lines (slurp "resources/input-3.txt")))

(defn calculate-toboggan-positions
  [right down]
  (let [rows (/ (count input-3) down)]
    (as-> (take rows (iterate (partial + right) 0)) <>
      (zipmap (map #(keyword (str %)) (take rows (iterate (partial + down) 0)))
              (map #(if (> % 30) (mod % 31) %) <>)))))

(defn calculate-tree-hits
  [right down]
  (let [positions (calculate-toboggan-positions right down)]
    (for [k (keys positions)]
      (let [position (k positions)
            row (read-string (last (str/split (str k) #":")))
            c (subs (get input-3 row) position (inc position))]
        (when (= "#" c)
          c)))))

(defn only-tree-hits
  [right down]
  (remove nil? (calculate-tree-hits right down)))

(defn tree-hit-count
  [right down]
  (count (only-tree-hits right down)))

(def slopes
  [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn tree-hits-in-slopes
  []
  (for [s slopes]
    (tree-hit-count (first s) (last s))))

(def tree-hit-multiple
  (reduce * (tree-hits-in-slopes)))

(defn solve-day-3
  []
  (println "Solutions for Day 3:")
  (println "  A:" (tree-hit-count 3 1))
  (println "  B:" tree-hit-multiple))

(def input-4
  (as-> (slurp "resources/input-4.txt") <>
    (str/split <> #"\n\n")
    (map #(str/replace % #"\n" " ") <>)
    (map str/trimr <>)
    (map #(str/split % #" ") <>)
    (for [s <>] (map #(str/split % #":") s))
    (for [s <>] (zipmap (map #(keyword (first %)) s) (map #(last %) s)))))

(defn valid-nof?
  [passport]
  (when (= 7 (count (keys (dissoc passport :cid))))
    true))

(defn is-year?
  [s]
  (when (and (= 4 (count s))
             (empty? (re-find #"[^0-9]" s)))
    true))

(defn valid-byr?
  [passport]
  (let [byr (:byr passport)]
    (when (and (is-year? byr)
               (<= 1920 (read-string byr) 2002))
      true)))

(defn valid-iyr?
  [passport]
  (let [iyr (:iyr passport)]
    (when (and (is-year? iyr)
               (<= 2010 (read-string iyr) 2020))
      true)))

(defn valid-eyr?
  [passport]
  (let [eyr (:eyr passport)]
    (when (and (is-year? eyr)
               (<= 2020 (read-string eyr) 2030))
      true)))

(defn hgt-within-range?
  [height unit]
  (case unit
    "cm" (when (<= 150 height 193) true)
    "in" (when (<= 59 height 76) true)
    nil))

(defn valid-hgt?
  [passport]
  (let [h (:hgt passport)]
    (when h
      (let[ending (re-find #"in|cm" h)
           measure (read-string (if (seq ending)
                                  (first (str/split h (re-pattern ending)))
                                  h))]
        (when (and (seq ending)) (hgt-within-range? measure ending)
          true)))))

(defn valid-hcl?
  [passport]
  (let [c (:hcl passport)]
    (when (and (= 7 (count c))
               (= \# (first c))
               (empty? (re-find #"[^0-9a-f]" (subs c 1))))
      true)))

(def valid-ecls ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])

(defn valid-ecl?
  [passport]
  (when (seq (filter #(re-find
                       (re-pattern (str "^" (:ecl passport) "$"))
                       %)
                     valid-ecls))
    true))

(defn valid-pid?
  [passport]
  (let [p (:pid passport)]
    (when (and (= 9 (count p))
               (empty? (re-find #"[^0-9]" p)))
      true)))

(defn count-all-passports-with-valid-nof
  []
  (count (remove nil? (for [p input-4] (valid-nof? p)))))

(defn count-all-valid-passports
  []
  (count (remove nil? (for [p input-4]
                        (when (and (valid-nof? p)
                                   (valid-byr? p)
                                   (valid-iyr? p)
                                   (valid-eyr? p)
                                   (valid-hgt? p)
                                   (valid-hcl? p)
                                   (valid-ecl? p)
                                   (valid-pid? p))
                          p)))))

(defn solve-day-4
  []
  (println "Solutions for Day 4:")
  (println "  A:" (count-all-passports-with-valid-nof))
  (println "  B:" (count-all-valid-passports)))

(defn fblr-to-01
  [s]
  (for [x s]
    (case x
      \F 0
      \B 1
      \L 0
      \R 1
      nil)))

(def input-5
  (as-> (slurp "resources/input-5.txt") <>
    (str/split-lines <>)
    (for [s <>] [(subs s 0 7) (subs s 7)])))

(def binary-input-5
  (for [s input-5] (flatten (map fblr-to-01 s))))

(defn find-row-or-seat
  ([b range]
   (let [range-length (count range)]
     (if (>= 1 range-length)
       (first range)
       (if (= 0 (first b))
         (find-row-or-seat (drop 1 b) (drop-last (/ range-length 2) range))
         (find-row-or-seat (drop 1 b) (drop (/ range-length 2) range))))))
  ([b]
   (if (= 3 (count b))
     (find-row-or-seat b (range 0 8))
     (find-row-or-seat b (range 0 128)))))

(defn find-all-rows-and-seats
  []
  (for [t binary-input-5]
    [(find-row-or-seat t) (find-row-or-seat (drop 7 t))]))

(defn calculate-seat-ids
  []
  (for [i (find-all-rows-and-seats)]
    (+ (* 8 (first i)) (last i))))

(defn highest-seat-id
  []
  (last (sort (calculate-seat-ids))))

(defn find-correct-seat
  []
  (first (difference (set (range 12 859)) (set (calculate-seat-ids)))))

(defn solve-day-5
  []
  (println "Solutions for Day 5:")
  (println "  A:" (highest-seat-id))
  (println "  B:" (find-correct-seat)))

(def input-6 (slurp "resources/input-6.txt"))

(def solution-6-a
  (as-> input-6 <>
    (str/split <> #"\n\n")
    (map #(str/replace % #"\n" "") <>)
    (map set <>)
    (map count <>)
    (reduce + <>)))

(def solution-6-b
  (as-> input-6 <>
    (str/split <> #"\n\n")
    (map #(str/replace % #"\n" " ") <>)
    (map str/trimr <>)
    (map #(str/split % #" ") <>)
    (for [g <>] (map set g))
    (map #(apply intersection %) <>)
    (map count <>)
    (reduce + <>)))

(defn solve-day-6
  []
  (println "Solutions for Day 6:")
  (println "  A:" solution-6-a)
  (println "  B:" solution-6-b))

(def input-7
  (str/split (slurp "resources/input-7.txt") #"\n"))

(def bag-rules
  (as-> input-7 <>
    (map #(str/replace % #"\,|bags|bag|contain|\." "") <>)
    (map #(str/replace % #"\s{2,3}" " ") <>)
    (map str/trimr <>)
    (map #(str/split % #" ") <>)
    (for [r <>] (map #(if (= 1 (count %)) (read-string %) %) r))
    (map #(partition-by int? %) <>)
    (for [r <>]
      (hash-map (keyword (str (first (first r)) "-"
                              (second (first r))))
                (map #(hash-map (keyword (str (second %) "-" (last %)))
                                (first %))
                     (partition 3 (flatten (rest r))))))))

(defn -main
  [d]
  (case (read-string d)
    1 (solve-day-1)
    2 (solve-day-2)
    3 (solve-day-3)
    4 (solve-day-4)
    5 (solve-day-5)
    6 (solve-day-6)
    (die 1 "Incorrect day provided.")))
