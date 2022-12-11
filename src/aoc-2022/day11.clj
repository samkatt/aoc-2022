(ns aoc-2022.day11
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-str "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")
(def input-str (slurp (io/reader (io/resource "input11.txt"))))

(def char->operation {"+" + "-" - "*" * "/" /})
(char->operation "+") ; #function[clojure.core/+]

(defn char->operand
  "Takes in a string representation of operands and returns a lambda that will do the right thing"
  [operand-str] (if (= "old" operand-str)
                  identity
                  (fn [_] (Integer/parseInt operand-str))))
((char->operand "old") 23) ; 23
((char->operand "-22") :unimportant-value) ; -22

(defn parse-monkey
  "Takes in a string describing a monkey and parses it as a data structure"
  [str] (let [lines (str/split str #"\n")
              monkey-id (re-find #"\d+" (first lines))
              items-holding (map #(Integer/parseInt %) (re-seq #"\d+" (second lines)))
              [operand1 operation operand2] (str/split (second (str/split (nth lines 2) #"= ")) #"\s")
              divisible-test-number (Integer/parseInt (re-find #"\d+" (nth lines 3)))
              monkey-target-if-true (Integer/parseInt (re-find #"\d+" (nth lines 4)))
              monkey-target-if-false (Integer/parseInt (re-find #"\d+" (nth lines 5)))] {:id monkey-id
                                                                                         :items (vec items-holding)
                                                                                         :operation {:op (char->operation operation)
                                                                                                     :operand1 (char->operand operand1)
                                                                                                     :operand2 (char->operand operand2)}
                                                                                         :test {:divisible-test-number divisible-test-number
                                                                                                :then-id monkey-target-if-true
                                                                                                :else-id  monkey-target-if-false}}))
(parse-monkey "Monkey 0:
 Starting items: 79, 98
 Operation: new = old * 19
 Test: divisible by 23
 If true: throw to monkey 2
 If false: throw to monkey 3")
; {:id "0",
;  :items (79 98),
;  :operation
;  {:op #function[clojure.core/*],
;   :operand1 #function[clojure.core/identity],
;   :operand2 #function[aoc-2022.day11/char->operand/fn--13047]},
;  :test
;  {:condition #function[aoc-2022.day11/parse-monkey/fn--13101],
;   :true 2,
;   :false 3}}
(map parse-monkey (str/split demo-str #"\n\n"))

(defn apply-turn
  "runs a turn of our monkey game starting in `state` doing turn `turn-id`"
  [state turn-id] (let [playing-monkey (nth state turn-id)
                        items (:items playing-monkey)
                        {:keys [op operand1 operand2]} (:operation playing-monkey)
                        {:keys [divisible-test-number then-id else-id]} (:test playing-monkey)]
                    (assoc-in (assoc-in (reduce (fn [state item] (let [worry (unchecked-divide-int (op (operand1 item) (operand2 item)) 3)
                                                                       target-monkey (if (= 0 (mod worry divisible-test-number)) then-id else-id)]
                                                                   (update-in state [target-monkey :items] conj worry)))
                                                state items) [turn-id :items] []) [turn-id :inspected-items] items)))

(apply-turn (mapv parse-monkey (str/split demo-str #"\n\n")) 0)

(defn get-monkey->inspected-item-count
  "doc-string" [state] (mapv #(count (:inspected-items %)) state))

(defn run-monkey-business
  [input-str apply-turn-fn n-rounds]
  (let [initial-state (mapv parse-monkey (str/split input-str #"\n\n"))
        n-turns (count initial-state)]
    (map
     (partial apply +)
     (apply
      (partial mapv vector)
      (take-nth n-turns (map
                         get-monkey->inspected-item-count
                         (reductions
                          apply-turn-fn
                          initial-state
                          (flatten (repeat n-rounds (range n-turns))))))))))

(apply * (take 2 (reverse (sort (run-monkey-business demo-str apply-turn 20))))) ; 10605
(println "Part 1:" (apply * (take 2 (reverse (sort (run-monkey-business input-str apply-turn 20)))))) ; 117624

; part 2 
(defn apply-turn-2
  "runs a turn of our monkey game starting in `state` doing turn `turn-id`"
  [state turn-id] (let [playing-monkey (nth state turn-id)
                        test-multiplied (apply * (map #(get-in % [:test :divisible-test-number]) state))
                        items (:items playing-monkey)
                        {:keys [op operand1 operand2]} (:operation playing-monkey)
                        {:keys [divisible-test-number then-id else-id]} (:test playing-monkey)]
                    (assoc-in (assoc-in (reduce (fn [state item] (let [worry (mod (op (operand1 item) (operand2 item)) test-multiplied)
                                                                       target-monkey (if (= 0 (mod worry divisible-test-number)) then-id else-id)]
                                                                   (update-in state [target-monkey :items] conj worry)))
                                                state items) [turn-id :items] []) [turn-id :inspected-items] items)))

(apply * (take 2 (reverse (sort (run-monkey-business demo-str apply-turn-2 10000))))) ; 2713310158
(println "Part 2:" (apply * (take 2 (reverse (sort (run-monkey-business input-str apply-turn-2 10000)))))) ; 16792940265
 
