(ns aoc-2022.day01
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def demo-str "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn input->inventory
  "Takes in a string representing calories per elf and returns list of list of calories"
  [str]
  (map #(map read-string (re-seq #"\d+" %)) (str/split str #"\n\n")))

(def demo-inventory (input->inventory demo-str))
; ((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000))

(defn inventory->calories
  "Takes in a [[c1 c2] [c c] ...] list of calories and returns the sum of each"
  [inv]
  (map (partial apply +) inv))

(def demo-calories (inventory->calories demo-inventory))
; (6000 4000 11000 24000 10000)

(apply max demo-calories) ; 24000

(def puzzle01-calories
  (inventory->calories (input->inventory (slurp (io/reader (io/resource "input01.txt"))))))

; part 1!
(println "Part 1:" (apply max puzzle01-calories)) ; 67450

; part 2
(apply + (take-last 3 (sort demo-calories))) ; 45000

(println "Part 2:" (apply + (take-last 3 (sort puzzle01-calories)))) ; 199357
