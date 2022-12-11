(ns aoc-2022.day06
  (:require
   [clojure.java.io :as io]))

(def demo-str "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def input-str (slurp (io/reader (io/resource "input06.txt"))))

(def str->chars (partial vec))

(defn find-marker
  [n-distinct-chars str] (->> str
                              str->chars
                              (partition n-distinct-chars 1)
                              (map #(count (set %)))
                              (take-while (partial not= n-distinct-chars))
                              count
                              (+ n-distinct-chars)))

(find-marker 4 demo-str) ; 7
(find-marker 4 "nppdvjthqldpwncqszvftbrmjlhg") ; 6
(find-marker 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ; 10
(find-marker 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ; 11

(println "Part 1:" (find-marker 4 input-str)) ; 1766

(find-marker 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb") ; 19
(find-marker 14 "bvwbjplbgvbhsrlpgdmjqwftvncz") ; 23
(find-marker 14 "nppdvjthqldpwncqszvftbrmjlhg") ; 23
(find-marker 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ; 29
(find-marker 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ; 26

(find-marker 14 input-str) ; 2383
