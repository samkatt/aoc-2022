(ns aoc-2022.day13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

; inputs
(def demo-input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")
(def puzzle-input (slurp (io/reader (io/resource "input13.txt"))))

; parsing

(defn input-str->input-data
  "Reads in a string representation of the input and returns as data.
 In particular, will return a vector of pairs of packets"
  [input-str] (map #(map read-string (str/split % #"\n")) (str/split input-str #"\n\n")))
(def demo-data (input-str->input-data demo-input))
; (([1 1 3 1 1] [1 1 5 1 1])
;  ([[1] [2 3 4]] [[1] 4])
;  ([9] [[8 7 6]])
;  ([[4 4] 4 4] [[4 4] 4 4 4])
;  ([7 7 7 7] [7 7 7])
;  ([] [3])
;  ([[[]]] [[]])
;  ([1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9]))

(def puzzle-data (input-str->input-data puzzle-input))

(defn right-order?
  "Returns whether two packets are in the right order.
  We encode this by returning true or false if the correct (or incorrect) order is found, `nil` if undecided
  Packets are in the right oder if:
  (1) when inputs are integers:
    - left < right -> right order
    - left > right -> wrong order
    - else: no verdict
  (2) when inputs are lists: (sequentially compare both elements)
    - if both empty: no verdict
    - if left is empty: right order
    - if right is empty: wrong order
    - compare first element of both lists and return their decision
    - if no verdict: continue with second elements
  (3) when one is integer and other is not:
    create list out of integer and compare as lists"
  [l r]
  (cond
    (and (number? l) (number? r)) (if (= l r)
                                    nil
                                    (< l r))
    (and (seqable? l) (seqable? r)) (cond
                                      ; easy cases: at least one is empty -> no decision
                                      (and (empty? l) (empty? r)) nil
                                      (empty? l) true
                                      (empty? r) false
                                      ; compare first elements, return decision if decided otherwise continue with tail
                                      :else (let [first-order (right-order? (first l) (first r))]
                                              (if (some? first-order)
                                                first-order
                                                (recur (rest l) (rest r)))))
    ; if the type differ, cast the integer to a list
    (seqable? r) (recur (list l) r)
    (seqable? l) (recur l (list r))))

(right-order? 1 1) ; nil
(right-order? 0 1) ; true
(right-order? 0 -1) ; false
(right-order? [] []) ; nil
(right-order? [1] []) ; false
(right-order? [] [1]) ; true
(right-order? [1] [1]) ; nil
(right-order? [1] [2]) ; true
(right-order? [2] [1]) ; false
(right-order? [1 2] [1 1]) ; false
(right-order? [1 1] [1 2]) ; true

(right-order? [1 1 3 1 1] [1 1 5 1 1]) ; true
(right-order? [[1] [2 3 4]] [[1] 4]) ; true

(filter second (map-indexed #(vector (inc %1) (apply right-order? %2)) demo-data))  ; ([1 true] [2 true] [4 true] [6 true])

(def puzzle-solution (filter second (map-indexed #(vector (inc %1) (apply right-order? %2)) puzzle-data)))
(println "Part 1:" (apply + (map first puzzle-solution))) ; 4734

; part 2

; now we need to arrange all packets in the correct order
; for this we need to sort them, according to the `right-order` comparator
(def demo-part-2-ordered (sort right-order? (conj (apply concat demo-data) [[2]] [[6]])))
; ([]
;  [[]]
;  [[[]]]
;  [1 1 3 1 1]
;  [1 1 5 1 1]
;  [[1] [2 3 4]]
;  [1 [2 [3 [4 [5 6 0]]]] 8 9]
;  [1 [2 [3 [4 [5 6 7]]]] 8 9]
;  [[1] 4]
;  [[2]]
;  [3]
;  [[4 4] 4 4]
;  [[4 4] 4 4 4]
;  [[6]]
;  [7 7 7]
;  [7 7 7 7]
;  [[8 7 6]]
;  [9])

(def puzzle-part-2-ordered (sort right-order? (conj (apply concat puzzle-data) [[2]] [[6]])))

(apply * (map #(inc (.indexOf demo-part-2-ordered %)) [[[2]] [[6]]])) ; 140
(println "Part 2:" (apply * (map #(inc (.indexOf puzzle-part-2-ordered %)) [[[2]] [[6]]]))) ; 21836
