(ns aoc-2022.day02
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(def char->play
  {\A :rock \B :paper \C :scissor \X :rock \Y :paper \Z :scissor})

(char->play \A) ; :rock

(def demo-input "A Y
B X
C Z")

(defn line->play
  "Translates 'A Y' into [:shape :shape]"
  [[opponent _ you]] [(char->play opponent) (char->play you)])

(defn input->plays
  "Translates string of plays 'A Y\nB Z....' into a sequence of [:shape :shape] "
  [input] (map line->play (str/split input #"\n")))

(def demo-plays (input->plays demo-input)) ; ([:rock :paper] [:paper :rock] [:scissor :scissor])

(def outcome->score
  {:loss 0 :draw 3 :win 6})

(outcome->score :draw) ; 3

(def shape->score
  {:rock 1 :paper 2 :scissor 3})

(shape->score :scissor) ; 3

(defn play->round
  "Translates [:shape :shape] into [:shape :outcome]"
  [[opponent you]]
  (if (= opponent you)
    [you :draw]
    (if (#{[:rock :scissor] [:scissor :paper] [:paper :rock]} [opponent you]) [you :loss] [you :win])))

(play->round [:rock :rock]) ; [:rock :draw]
(play->round [:paper :scissor]) ; [:scissor :win]
(play->round [:rock :scissor]) ; [:scissor :loss]

(defn round->score
  "Translates a round into its score"
  [shape outcome] (+ (shape->score shape) (outcome->score outcome)))

(round->score :rock :draw) ; 4

(defn rounds->scores
  "Turns [[:shape :outcome] ...] into [score score ...]"
  [rounds] (map #(apply round->score %) rounds))

(rounds->scores [[:rock :draw] [:scissor :loss]]) ; (4 3)
(rounds->scores (map play->round demo-plays)) ; (8 1 6)

(def puzzle-input (slurp (io/reader (io/resource "input02.txt"))))
(println "Part 1:"
         (apply + (rounds->scores (map play->round (input->plays puzzle-input))))) ; 11150

(def char->outcome
  {\X :loss \Y :draw \Z :win})

(char->outcome \Z) ; :win

(defn match-opponent
  "Gives back the desired shape given the opponents and desired outcome"
  [shape desired-outcome] (cond
                            (= :draw desired-outcome) shape
                            (= :win desired-outcome) ({:rock :paper :paper :scissor :scissor :rock} shape)
                            :else ({:paper :rock :scissor :paper :rock :scissor} shape)))

(match-opponent :rock :loss) ; :scissor
(match-opponent :scissor :draw) ; :scissor
(match-opponent :paper :loss) ; :rock

(defn line->round-2
  "Translates 'A Y' into [:shape :outcome]"
  [[shape _ outcome-char]] (let [desired-outcome (char->outcome outcome-char)]
                             [(match-opponent (char->play shape) desired-outcome) desired-outcome]))

(defn input->rounds
  "Translates string of plays 'A Y\nB Z....' into a sequence of [:shape :outcome] "
  [input] (map line->round-2 (str/split input #"\n")))

(line->round-2 "B Z") ; [:scissor :win]

(println "Part 2:"
         (apply + (rounds->scores (input->rounds puzzle-input)))) ; 8295
