(defproject advent_2020 "0.1.0-SNAPSHOT"
  :description "Advent of code - 2020"
  :url "https://github.com/johanthoren/advent_of_code_2020"
  :license {:name "ISC License"
            :url "none"
            :year 2020
            :key "isc"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot advent-2020.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
