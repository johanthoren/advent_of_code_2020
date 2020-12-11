(ns advent-2020.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str])
  (:gen-class))

(def input-1
  (map read-string (str/split-lines (slurp "resources/input-1.txt"))))

(defn add-to-2020
  []
  ;;(reduce * (set (remove nil? (flatten (for [a input-1] (map #(when (= 2020 (+ a %)) [a %]) input-1)))))))
  (as-> input-1 <>
      (for [a <>] (map #(when (= 2020 (+ a %)) [a %]) <>))
      (flatten <>)
      (remove nil? <>)
      (set <>)
      (reduce * <>)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
