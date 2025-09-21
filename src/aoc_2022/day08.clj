(ns aoc-2022.day08
  (:require
   [aoc-2022.utils :refer [v+]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-str "30373
25512
65332
33549
35390")
(def input-str (slurp (io/reader (io/resource "input08.txt"))))

; interpret map as tree heights
(defn str->height-map
  [input-str] (mapv (fn [str] (mapv #(Integer/parseInt %) (re-seq #"\d" str))) (str/split input-str #"\n")))
(def demo-height-map (str->height-map demo-str)) ; [[3 0 3 7 3] [2 5 5 1 2] [6 5 3 3 2] [3 3 5 4 9] [3 5 3 9 0]]

(defn on-edge?
  "Computes whether `next-location` lies on the edge of `height-map`"
  [height-map location]
  (or (= 0 (first location))
      (= 0 (second location))
      (= (first location) (dec (count height-map)))
      (= (second location) (dec (count (first height-map))))))
(on-edge? demo-height-map [1 1]) ; false
(on-edge? demo-height-map [1 0]) ; true
(on-edge? demo-height-map [4 1]) ; true
(on-edge? demo-height-map [1 4]) ; true
(on-edge? demo-height-map [2 3]) ; false

; part 1 naively figure out whether trees are visible
; first see whether a tree is visible from a particular direction

(def NORTH [-1 0])
(def SOUTH [1 0])
(def EAST [0 1])
(def WEST [0 -1])

(defn is-tree-visible-from?
  "Computes recursively whether a tree is visible from a particular direction"
  ([height-map location direction]
   (is-tree-visible-from? height-map (get-in height-map location) location direction))
  ([height-map height location direction]
   (if (on-edge? height-map location)
     true
     (let [next-location (v+ location direction)]
       ; we have reached the edge, tree is visible!
       ; tree is visible if next location is lower and next location is visible
       (and (> height (get-in height-map next-location))
            (recur height-map height next-location direction))))))

(is-tree-visible-from? demo-height-map [1 2] NORTH) ; true
(is-tree-visible-from? demo-height-map [1 2] EAST) ; true
(is-tree-visible-from? demo-height-map [1 2] WEST) ; false

(defn is-tree-visible?
  "Computes, naively, whether a particular tree is visible given the map"
  [height-map location]
  (some identity (for [dir [NORTH EAST SOUTH WEST]]
                   (is-tree-visible-from? height-map location dir))))
(is-tree-visible? demo-height-map [0 0]) ; true
(is-tree-visible? demo-height-map [1 1]) ; true
(is-tree-visible? demo-height-map [1 3]) ; nil
(is-tree-visible? demo-height-map [2 1]) ; true
(is-tree-visible? demo-height-map [2 2]) ; nil
(is-tree-visible? demo-height-map [2 3]) ; true
(is-tree-visible? demo-height-map [3 1]) ; nil
(is-tree-visible? demo-height-map [3 2]) ; true
(is-tree-visible? demo-height-map [3 3]) ; nil

(defn height-map->visibility-map "Give back a visibility map"
  [height-map] (for [y (range (count height-map))
                     x (range (count (first height-map)))]
                 (is-tree-visible? height-map [y x])))

(def demo-visibility-map (height-map->visibility-map demo-height-map))
(count (filter identity demo-visibility-map))

(def input-visibility-map (height-map->visibility-map (str->height-map input-str)))
(println "Part 1" (count (filter identity input-visibility-map))) ; 1705

; Part 2
; Here we want to find the 'best spot'
; The best spot is the tree from which the most (other) trees can be seen  (multiplied in each direction)
; This is computed by counting all the trees visible: until edge or a tree of at least the same height is encountered

; First we help ourselves by knowing when we are *out of bound*
(defn out-of-bounds?
  "Returns whether a given location is out of bounds"
  [height-map [y x]]
  (or (> 0 y)
      (> 0 x)
      (= y (count height-map))
      (= x (count (first height-map)))))
(out-of-bounds? demo-height-map [-1 1]) ; true
(out-of-bounds? demo-height-map [0 0]) ; false
(out-of-bounds? demo-height-map [4 4]) ; false
(out-of-bounds? demo-height-map [3 5]) ; true
(out-of-bounds? demo-height-map [3 0]) ; false

; We will compute the scenic thing 'per tree', but first I suppose we should learn to do so per direction
(defn visible-trees-in-direction
  "Returns the number of trees visible from a particular direction"
  ([height-map location direction]
   (visible-trees-in-direction height-map location direction (get-in height-map location) 0))
  ([height-map location direction height acc]
   ; Recursive approach, where there are three options:
   (let [next-location (v+ location direction)]
     (cond
       ; option 1: we are on the edge, then we see no more trees and return our accumulation
       (out-of-bounds? height-map next-location) acc
       ; option 2: the tree in the next location blocks us: we can see only that tree extra
       (<= height (get-in height-map next-location)) (inc acc)
       :else (recur height-map next-location direction height (inc acc))))))
(visible-trees-in-direction demo-height-map [0 0] NORTH) ; 0
(visible-trees-in-direction demo-height-map [0 0] EAST) ; 2
(visible-trees-in-direction demo-height-map [0 0] SOUTH) ; 2
(visible-trees-in-direction demo-height-map [1 2] NORTH) ; 1
(map (partial visible-trees-in-direction demo-height-map [1 2]) [NORTH WEST EAST SOUTH]) ; (1 1 2 2)
(map (partial visible-trees-in-direction demo-height-map [3 2]) [NORTH WEST SOUTH EAST]) ; (2 2 1 2)

(defn visible-trees-per-direction
  "Returns number of visible trees given a location"
  [height-map location] (map (partial visible-trees-in-direction height-map location) [NORTH EAST SOUTH WEST]))
(visible-trees-per-direction demo-height-map [1 2]) ; (1 2 2 1)
(visible-trees-per-direction demo-height-map [3 2]) ; (2 2 1 2)

(defn all-locations 
  "Gives back all legal locations of a height-map"
  [height-map] (for [y (range (count height-map))
                     x (range (count (first height-map)))]
                 [y x]))
(take 6 (all-locations demo-height-map)) ; ([0 0] [0 1] [0 2] [0 3] [0 4] [1 0])

(apply max-key second (map #(into [] [% (apply * (visible-trees-per-direction demo-height-map %))]) (all-locations demo-height-map))) ; [[3 2] 8]
(def input-height-map (str->height-map input-str))
(println "Part 2:" (apply max-key second (map #(into [] [% (apply * (visible-trees-per-direction input-height-map %))]) (all-locations input-height-map)))) ; [[16 40] 371200]
