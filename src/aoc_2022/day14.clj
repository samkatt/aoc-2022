(ns aoc-2022.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-input "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")
(def puzzle-input (slurp (io/reader (io/resource "input14.txt"))))

; parsing

; We are translating 'rock lines' into a 'occupancy grid'
; In particular, we just want to store and look up all 
; locations (y, x) that are occupied.

(defn str->ranges
  "translates string inputs into lists of lists of ranges
  E.g. each line '498,4 -> 498,6 -> 496,6' becomes ((498 4) (498 6) (496 6))"
  [input-str]
  (map
   #(partition 2 (map read-string (re-seq #"\d+" %)))
   (str/split input-str #"\n")))
(first (str->ranges demo-input)) ; ((498 4) (498 6) (496 6))

(def demo-ranges (str->ranges demo-input))
(def puzzle-ranges (str->ranges puzzle-input))

(defn range-pair->locations
  "Translate a pair of ranges into a sequence of locations
  E.g. ((498 4) (498 6)) -> ((498 4) (498 5) (498 6))"
  [[x1 y1] [x2 y2]]
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))
(range-pair->locations [498 4] [498 6]) ; ([498 4] [498 5] [498 6])
(range-pair->locations [498 6] [496 6]) ; ([496 6] [497 6] [498 6])

(defn ranges->locations
  "Translates a list of ranges into locations
  E.g. ((498 4) (498 6) (496 6)) -> #{(498 4) (498 5) (498 6) (497 6) (496 6))} "
  [ranges] (mapcat
            (partial apply range-pair->locations)
            ; zip specifically to make sure all pairs are looked at
            (zipmap (drop-last ranges) (rest ranges))))

(def demo-locations (set (mapcat ranges->locations demo-ranges)))
(count demo-locations) ; 20

(def puzzle-locations (set (mapcat ranges->locations puzzle-ranges)))

(defn locations->abyss
  "Returns what 'level' abyss is on.
  In other words, given a list of locations, will return
  what the lowest y coordinate is."
  [locations] (apply max (map second locations)))
(locations->abyss demo-locations) ; 9

; now that we have all locations, we start simulating sand falling
; first we list the possible next locations (`next-sand-locations`)
; then we use those to simulate a single step
; then we continue running that until:
;   - there is no possible next location ('stuck'), or
;   - the location reached lower than the lowest occupied grid (starts flowing into abyss)

(defn next-sand-locations
  "This will give us, with priority, the next possible three spots sand will fall to
  I.e. (down down-left down-right)"
  [[x y]] [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]])
(next-sand-locations [500 0]) ; [[500 1] [499 1] [501 1]]

(defn sand-step
  "Simulates a 'step' of sand: fall.
  Returns its next location, or `nil`"
  [occupied-locations loc] (first (filter #(not (contains? occupied-locations %)) (next-sand-locations loc))))
(sand-step demo-locations [500 0]) ; [500 1]
(sand-step demo-locations [500 7]) ; [500 8]
(sand-step demo-locations [500 8]) ; nil
(sand-step demo-locations [498 3]) ; [497 4]
(sand-step demo-locations [503 3]) ; [504 4]

(defn sand-fall
  "Simulates a whole 'falling' of a single sand.
  Will return the final location of a (new) sand falling.
  This will either return the final place, or fall on the `abyss`"
  [occupied-locations abyss] (last (take-while
                                    #(and (some? %) (>= abyss (last %)))
                                    (iterate (partial sand-step occupied-locations) [500 0]))))
(sand-fall demo-locations (locations->abyss demo-locations)) ; [500 8]
(sand-fall (conj demo-locations [500 8]) (locations->abyss demo-locations)) ; [499 8]
(sand-fall demo-locations 6) ; [500 6]

(defn simulation
  "Runs the simulation of falling sand.
  Increasingly add sand to `occupied-locations` until `pred` returns falls"
  [pred occupied-locations floor]
  (loop [occupied-locations occupied-locations]
    (let [next-loc (sand-fall occupied-locations floor)]
      (if (pred next-loc)
        (recur (conj occupied-locations next-loc))
        occupied-locations))))

(defn part-1
  "Computes part-1, which is basically running `simulation` until `abyss` is reached."
  [occupied-locations] (let [floor (locations->abyss occupied-locations)]
                         (simulation #(> floor (second %)) occupied-locations floor)))

(- (count (part-1 demo-locations)) (count demo-locations)) ; 24
(println "Part 1:" (- (count (part-1 puzzle-locations)) (count puzzle-locations))) ; 1133

; part 2

(defn locations->abyss-part2
  "Returns what 'level' abyss is on for part 2: highest y + 2.
  In other words, given a list of locations, will return
  what the lowest y coordinate is."
  [locations] (+ 1 (locations->abyss locations)))

(defn part-2
  "Run `simulation` with `abyss-part2` floor until location [500 0] is reached"
  [locations] (simulation (partial not= [500 0]) locations (locations->abyss-part2 locations)))

(inc (- (count (part-2 demo-locations)) (count demo-locations))) ; 93
(println "Part 2:" (inc (- (count (part-2 puzzle-locations)) (count puzzle-locations)))) ; 27566
