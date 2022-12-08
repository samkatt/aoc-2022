(ns aoc-2022.day03
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io]))

(def demo-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn line->rucksack
  "Translates a 'vJrwpWtwJgWrhcsFMMfFFhFp' line into [[v J ...], [h c ...]] rucksack"
  [line] (let [items (vec (char-array line))
               half (/ (count items) 2)]
           [(subvec items 0 half) (subvec items half)]))

(line->rucksack (first (str/split demo-input #"\n")))
; [[\v \J \r \w \p \W \t \w \J \g \W \r]
;  [\h \c \s \F \M \M \f \F \F \h \F \p]]

(defn input->rucksacks
  "Translates input strings to rucksacks"
  [str-input] (map line->rucksack (str/split str-input #"\n")))

(first (input->rucksacks demo-input))
; [[\v \J \r \w \p \W \t \w \J \g \W \r]
;  [\h \c \s \F \M \M \f \F \F \h \F \p]]

(defn common-items-in-compartments
  "Returns the items in common in a rucksack compartment"
  [[c1 c2]] (set/intersection (set c1) (set c2)))

(common-items-in-compartments (first (input->rucksacks demo-input))) ; #{\p}

(defn char->int
  "Translates a character to its 'priority'"
  [c] (if (Character/isUpperCase c)
        (+ (- (int c) (int \A)) 27)
        (+ (- (int c) (int \a)) 1)))

(map char->int [\a \z \A \Z]) ; (1 26 27 52)

(defn input->part1
  "Takes in input string and outputs sum of priority"
  [str-input] (->> str-input
                   input->rucksacks
                   (mapcat common-items-in-compartments) ; (#{\p} #{\L} #{\P} #{\v} #{\t} #{\s})
                   (map char->int) ; (16 38 42 22 20 19)
                   (apply +))) ; 157

(input->part1 demo-input) ; 157
(println "Part 1:" (input->part1 (slurp (io/reader (io/resource "input03.txt"))))) ; 7553

(def input->group-inventory (comp (partial partition 3)
                                  (partial map flatten)
                                  (partial input->rucksacks)))

(defn group-inventory->badge
  "Gets the badge from a set of rucksacks"
  [rucksacks] (->> rucksacks
                   (map set) ; make set of each rucksack
                   (apply set/intersection) ; get intersection
                   seq
                   first))

(def input->part2 (comp (partial apply +)
                        (partial map char->int)
                        (partial map group-inventory->badge)
                        input->group-inventory))

(input->part2 demo-input) ; 70
(println "Part 2: " (input->part2 (slurp (io/reader (io/resource "input03.txt"))))) ; 2758
