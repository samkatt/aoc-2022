(ns aoc-2022.day09
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str]))

(Math/floorDiv 1 2)

(def demo-str "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")
(def input-str (slurp (io/reader (io/resource "input09.txt"))))

(defn str->moves [str] (map #(vector (first %) (Integer/parseInt (re-find #"\d+" %))) (str/split str #"\n")))
(take 2 (str->moves demo-str)) ; ([\R 4] [\U 4])

(def initial-state {:H [0 0] :T [[0 0]] :cells #{}})

(def dir->pos {\U [0 1] \D [0 -1] \L [-1 0] \R [1 0]})
(dir->pos \U) ; [0 1]

(defn vec+
  "Adds two vectors"
  [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(vec+ [5 -2] [-1 3]) ; [4 1]

(defn adjacent?
  "Returns whether two positions are adjacent"
  [[x1 y1] p2] ((into #{} (for [x (range -1 2) y (range -1 2)]
                            [(+ x1 x) (+ y1 y)])) p2))
(adjacent? [3 1] [1 1])

(def jump {0 0
           1 1
           -1 -1
           2 1
           -2 -1})

(defn update-tail-position
  "Gets new tail position given head"
  ([[xH yH] [xT yT :as T]]
   (let [x-diff (- xH xT) y-diff (- yH yT)]
     (if (empty? (set/intersection #{2 -2} (into #{} [x-diff y-diff])))
       T ; H and T are 'touching'
       [(+ xT (jump x-diff)) (+ yT (jump y-diff))])))
  ([H second & tail]
   (loop [updated-positions [(update-tail-position H second)] tail tail]
     (if (empty? tail)
       updated-positions
       (recur
        (conj updated-positions (update-tail-position (last updated-positions) (first tail)))
        (rest tail))))))

(update-tail-position [0 0] [-1 1]) ; [-1 1]
(update-tail-position [0 0] [1, -2]) ; [0 -1]
(update-tail-position [0 0] [2, -1]) ; [1 0]
(update-tail-position [2 1] [0 0] [0 0]) ; [[1 1] [0 0]]
(update-tail-position [3 2] [1 1] [0 0]) ; [[2 2] [1 1]]

(defn apply-move
  "Applies `move` to `state`"
  ([state dir] (let [H' (vec+ (:H state) (dir->pos dir))
                     updated-positions (apply update-tail-position H' (:T state))
                     ; pack T' back if necessary
                     T' (if (vector? (first updated-positions)) updated-positions [updated-positions])]
                 {:H H'
                  :T T'
                  :cells (conj (:cells state) (last T'))}))
  ([state dir n] (if (= 0 n)
                   state
                   (recur (apply-move state dir) dir (dec n)))))

(reduce (fn [s [dir n]] (apply-move s dir n)) initial-state (str->moves demo-str))
; {:H [2 2],
;  :T [1 2],
;  :cells #{[4 3] [2 2] [0 0] [1 0] [3 3] [3 4] [4 2] [3 0] [4 1] [2 4] [2 0] [1 2] [3 2]}}

(println "Part 1:"
         (count (:cells (reduce (fn [s [dir n]] (apply-move s dir n)) initial-state (str->moves input-str)))))
; (out) Part 1: 6339

(def initial-state-2 {:H [0 0] :T (repeat 9 [0 0]) :cells #{}})
(def demo-str-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(count (:cells (reduce (fn [s [dir n]] (apply-move s dir n)) initial-state-2 (str->moves demo-str-2)))) ; 36
(println "Part 2:" (count (:cells (reduce (fn [s [dir n]] (apply-move s dir n)) initial-state-2 (str->moves input-str))))) 
; (out) Part 1: 2541
