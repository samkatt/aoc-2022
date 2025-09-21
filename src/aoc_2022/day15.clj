(ns aoc-2022.day15
  (:require
   [aoc-2022.utils :refer [manhattan]]
   [clojure.java.io :as io]))

; input saving

(def demo-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")
(def puzzle-input (slurp (io/reader (io/resource "input15.txt"))))

; parsing
; I think it is easiest to just store a map for each pair: {:s(ensor) :b(eacon) :d(istance)}
; In addition, maybe just ensure we have a set of all occupied spots, max/min y/x and max distance
; Resulting in {:occupied-spots #{} :pairs ({:s :b :d}) :max {:x :y :d}}

(defn parse-pair
  "Reads in 4 numbers and parses as source-beacon pair"
  [[xs ys xb yb]] {:s [xs ys] :b [xb yb]
                   :d (manhattan [xs ys] [xb yb])})

(defn parse-input-str
  "Reads in a (string) input and returns an appropriate data structure"
  [input-str]
  (let [all-numbers (map #(Integer/parseInt %) (re-seq #"-*\d+" input-str))
        all-x (take-nth 2 all-numbers)
        all-y (take-nth 2 (rest all-numbers))
        pairs (map parse-pair (partition 4 all-numbers))]
    {:occupied-spots (set (map vec (partition 2 all-numbers)))
     :pairs pairs
     :max {:x [(apply min all-x) (apply max all-x)]
           :y [(apply min all-y) (apply max all-y)]
           :d (apply max (map :d pairs))}}))
(keys (parse-input-str demo-input)) ; (:occupied-spots :pairs :max)

(def demo-data (parse-input-str demo-input))
(def puzzle-data (parse-input-str puzzle-input))

; Part 1: naive
; For each (relevant) point,
;   - go over all sensors
;   - check if distance is less than the sensors distance

(defn covered?
  "Checks if a coordinate is 'covered' by a sensor"
  [{:keys [s d]} coord]
  (>= d (manhattan s coord)))
(some #(covered? % [-3 10]) (:pairs (parse-input-str demo-input))) ; nil
(some #(covered? % [-2 10]) (:pairs (parse-input-str demo-input))) ; true

(defn possible-beacon?
  "Check if a coordinate can be a beacon (occupied or covered)"
  [{:keys [pairs occupied-spots]} coord]
  (or
   (occupied-spots coord)
   (not-any? #(covered? % coord) pairs)))
(possible-beacon? demo-data [-3 10]) ; true
(possible-beacon? demo-data [-2 10]) ; false
(possible-beacon? demo-data [2 10]) ; [2 10]

(defn width-range
  "Returns the range of x values that could possible have a beacon"
  [data]
  (range
   (- (get-in data [:max :x 0]) (get-in data [:max :d]))
   (+ (get-in data [:max :x 1]) (get-in data [:max :d]))))

(defn count-possible-beacons-in-row
  "Counts the possible beacons in `data` on `row`"
  [data row]
  (->> (width-range data)                      ; (x_0 .... x_n)
       (map #(possible-beacon? data [% row]))  ; (true true false false ....)
       (filter false?)                         ; (false ....)
       count))                                 ; n

(comment (count-possible-beacons-in-row demo-data 10)) ; 26

; Unfortunately this runs too slow (eventually gets the right answer though)
; (println "Puzzle input:" (count-possible-beacons-in-row puzzle-data 2000000))

; A wild alternative solution is to check, for each sensor,
; which positions are covered.
; Then we can take its set, and count it!
; A sensor covers part of the row if
; location + distance < row

; If so, we can probably find the range of coordinates it occupies
; In particular, the center of this segment is [s_x row]
; And it goes left and right exactly the same amount as `s_y -+ distance - row`
; Whether it is plus or minus depends on the 'direction'....
; Or no, we just do absolute of `s_y - row` and that minus distance

; So sensor [0 11] with distance 3 should cover 5 parts of row 10
; [-2 10] ... [2 10]

:input 10 {:s [0 11], :b [2 10], :d 3}
:output '([-2 10] [-1 10] [0 10] [1 10] [2 10])
(defn point->row-segment
  "Takes a `row` index and a `pair` and returns the covered segment"
  [row {:keys [s d]}]
  (let [[x y] s, segment-half-width (- d (Math/abs (- row y)))]
    (for [x (range
             (- x segment-half-width)
             (+ x (inc segment-half-width)))]
      [x row])))
(point->row-segment 10 {:s [0 11], :b [2 10], :d 3}) ; ([-2 10] [-1 10] [0 10] [1 10] [2 10])

; Now we want to gather _all_ segments (so map over points) and count the set

(defn row-coverage
  "Returns a set of all locations covered in `row`"
  [pairs row]
  (->> pairs
       (mapcat #(point->row-segment row %))  ; (() ([x y] [x y]) () ...)
       set))
(count (row-coverage (:pairs demo-data) 10)) ; 27

; Problem is that we actually count position where there are already beacons.
; So we simply find all beacons on the row and substract that from what we find.

(defn beacons-on-row
  "Takes in `pairs` and returns the position of beacons which are on `row`"
  [pairs row]
  (->> pairs
       (map :b)                     ; ([b_x b_y] ...)
       set
       (filter #(= row (second %))) ; ([2 10])
       ))
(beacons-on-row (:pairs demo-data) 10) ; ([2 10])

(defn part-1
  "Computes part 1: number of `row-coverage` - number of `beacons-on-row`"
  [{pairs :pairs} row]
  (-
   (count (row-coverage pairs row))
   (count (beacons-on-row pairs row))))
(part-1 demo-data 10) ; 26
(println "Part 1" (part-1 puzzle-data 2000000)) ; 4502208

; Part 2, find the distress signal.
; So within [0 0] [4000000 4000000] we must find the beacon.
; We know the beacon must be a place that is _not covered_.
; So we may be able to re-use `coverd?`

; The main issue is the super large search space.
; Since we _know_ there is exactly one, 
; it _must be_ just outside of one of the perimeters.
; So I propose to iterate over those.

:input {:s [0 11], :b [2 10], :d 2}
:output '([x1 y1] [x2 y2] ...)
(defn sensor->perimeter
  "Returns the perimeter of the input sensor (list of sequences)"
  [{:keys [s d]}]
  (let [d (inc d)
        [y x] s
        [min-x max-x] [(- x d) (+ x d)]
        [min-y max-y] [(- y d) (+ y d)]
        y-coords (concat (range min-y max-y) (range max-y min-y -1))
        x-coords (concat
                  (range x max-x)
                  (range max-x min-x -1)
                  (range min-x x -1))]
    (map vector y-coords x-coords)))
(count (sensor->perimeter {:s [0 11], :b [2 10], :d 2}))

; In this case we do not want to use `possible-beacon?`
; So we will re-use our `covered?` instead

(defn distress-signal?
  "Returns whether `coord` is our stress signal"
  [pairs coord]
  (not-any? #(covered? % coord) pairs))
(distress-signal? (:pairs demo-data) [14 11]) ; true
(distress-signal? (:pairs demo-data) [16 7]) ; false

(defn find-distress-signal
  [pairs grid-bound]
  (->> pairs
       (mapcat sensor->perimeter)
       (filter #(and (<= 0 (first %) grid-bound) (<= 0 (second %) grid-bound)))
       (map (fn [coord] [coord (distress-signal? pairs coord)]))
       (filter second)
       first
       first))
(find-distress-signal (:pairs demo-data) 20) ; [14 11]

(defn part-2 [data grid-bound]
  (let [[y x] (find-distress-signal (:pairs data) grid-bound)]
    (println "Part 2: " (+ x (* y 4000000)))
    [y x]))
(part-2 demo-data 20)
(part-2 puzzle-data 4000000) ;  [3446137 3204480]

(+ 3204480 (* 3446137 4000000)) ; 13784551204480

