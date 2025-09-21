(ns aoc-2022.day04
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io]))

(def str-demo "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")
(def str-input (slurp (io/reader (io/resource "input04.txt"))))

(defn line->ranges
  "Interprets single input line '51-88,52-87' into [set set]"
  [line] (let [[a b y z] (re-seq #"\d+" line)]
           [(set (range (Integer/parseInt a) (inc (Integer/parseInt b))))
            (set (range (Integer/parseInt y) (inc (Integer/parseInt z))))]))
(line->ranges "2-3,4-5")

(defn str->ranges
  "Interprets input as string and returns [[set set] [set set] ...]"
  [str] (map line->ranges (str/split str #"\n")))
(take 2 (str->ranges str-demo)) ; ([#{4 3 2} #{7 6 8}] [#{3 2} #{4 5}])

(defn sub-or-superset?
  "Returns whether s1 is either a sub or a superset of s2"
  [s1 s2] (or (set/subset? s1 s2) (set/superset? s1 s2)))
(sub-or-superset? #{1 2 3} #{1 2}) ; true

(println "Part 1:" (->> (str->ranges str-input)
                        (filter (partial apply sub-or-superset?))
                        count)) ; 487

(println "Part 2:" (->> (str->ranges str-input)
                        (map (partial apply set/intersection))
                        (filter not-empty)
                        count)) ; 849
