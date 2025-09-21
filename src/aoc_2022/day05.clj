(ns aoc-2022.day05
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def demo-str "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def input-str (slurp (io/reader (io/resource "input05.txt"))))

(defn line->crate-content
  "Translates a str describing crates into crate->content mapping"
  [line] (filter #(not= \space (second %)) (map-indexed vector (take-nth 4 (drop 1 line)))))
(line->crate-content "[Z]     [P]") ; ([0 \Z] [2 \P])

(defn add-content-to-crate
  "Adds a new `itm` to `crates` at crate `idx`"
  [crates [idx itm]] (assoc crates idx (conj (nth crates idx) itm)))
(add-content-to-crate [[\N] [\C] []] [0 \Z]) ; [[\N \Z] [\C] []]

(defn add-contents-to-crates
  "Processes a new mapping of crates to existing"
  [crates new-content] (reduce
                        add-content-to-crate
                        crates new-content))
(add-contents-to-crates [[\N] [\C] []] '([0 \Z] [2 \P])) ; [[\N \Z] [\C] [\P]]

(defn initial-crates-description->data-structure
  "Translates string describes initial crates into data structure"
  [initial-crate-description n] (vec (map
                                      (partial apply list) ; turn each stack of crates into a list
                                      (reduce
                                       add-contents-to-crates
                                       (vec (repeatedly n vector))  ; start with empty stack of crates
                                       (map line->crate-content initial-crate-description)))))
(initial-crates-description->data-structure '("    [D]    "
                                              "[N] [C]    "
                                              "[Z] [M] [P]") 3) ; [(\N \Z) (\D \C \M) (\P)]

(defn split-input-str
  "Splits input string into useful bits: [initial-stacks number-stacks moves]"
  [input-str] (let [lines (str/split input-str #"\n")]
                [(take-while #(not= " 1 " (apply str (take 3 %))) lines)
                 (Character/digit (second (reverse (last (take-while (partial not= "") lines)))) 10)
                 (drop-while #(not= "move" (apply str (take 4 %))) lines)]))
(split-input-str demo-str)
; [("    [D]    " "[N] [C]    " "[Z] [M] [P]")
;  3
 ; ("move 1 from 2 to 1"
 ;  "move 3 from 1 to 3"
 ;  "move 2 from 2 to 1"
 ;  "move 1 from 1 to 2")]

(defn moves-str->moves-data
  "Translates moves string description to actual data"
  [moves-str] (map vec (map (fn [i] (map #(Integer/parseInt %) (re-seq #"\d+" i))) moves-str)))
(moves-str->moves-data '("move 1 from 2 to 1"
                         "move 3 from 1 to 3")) ; ([1 2 1] [3 1 3])

(defn apply-move
  "Moves `n` crates from stack `source-idx` to stack `target-idx`"
  [crates [n source-idx target-idx]]
  (if (= 0 n)
    crates  ; base case: none to move
    (recur
     ; store 'real' indices (decrement) and figure out what the crate is
     (let [source-idx (dec source-idx)
           target-idx (dec target-idx)
           crate (first (nth crates source-idx))]
       (assoc crates ; update crates by removing from source and conj to target
              source-idx (rest (nth crates source-idx))
              target-idx (conj (nth crates target-idx) crate)))
     [(dec n) source-idx target-idx])))  ; recur with 1 less crate
(apply-move (apply-move ['(\N \Z) '(\D \C \M) '(\P)] [1 2 1]) [3 1 3])

(defn concatenate-top-of-stacks
  [crates] (apply str (map first crates)))

(let [[crates-descr n moves] (split-input-str demo-str)
      crates (initial-crates-description->data-structure crates-descr n)]
  (concatenate-top-of-stacks (reduce apply-move crates (moves-str->moves-data moves)))) ; "CMZ"

(println "Part 1:" (let [[crates-descr n moves] (split-input-str input-str)
                         crates (initial-crates-description->data-structure crates-descr n)]
                     (concatenate-top-of-stacks (reduce apply-move crates (moves-str->moves-data moves))))) ; "QPJPLMNNR"

(defn apply-move-9001
  "Moves `n` crates from stack `source-idx` to stack `target-idx` according to mover 9001"
  [crates [n source-idx target-idx]]
  (let [source-idx (dec source-idx)
        target-idx (dec target-idx)
        moving-crates (take n (nth crates source-idx))]
    (assoc crates
           source-idx (drop n (nth crates source-idx))
           target-idx (apply conj (nth crates target-idx) (reverse moving-crates)))))

(apply-move-9001 (apply-move-9001 ['(\N \Z) '(\D \C \M) '(\P)] [1 2 1]) [3 1 3]) ; [() (\C \M) (\D \N \Z \P)]

(println "Part 2:" (let [[crates-descr n moves] (split-input-str input-str)
                         crates (initial-crates-description->data-structure crates-descr n)]
                     (concatenate-top-of-stacks (reduce apply-move-9001 crates (moves-str->moves-data moves))))) ; BQDNWJPVJ
