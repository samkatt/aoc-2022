(ns aoc-2022.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def demo-str "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")
(def input-str (slurp (io/reader (io/resource "input07.txt"))))

; we define our system as a tree, where directories map from names to files/directories, and files to their size
; then we just maintain the current path
(def init-state {:system {} :path []})

(def split-program-to-commands #(drop 1 (str/split % #"\$ ")))
(first (split-program-to-commands demo-str)) ; "cd /\n"

(defn apply-command
  "Processes a new command given the current state"
  [{:keys [system path] :as state} command]
  (let [op (first (str/split command #"\s"))]
    (cond
      (= op "cd") (let [next-dir (second (str/split command #"\s"))]
                    (cond
                      (= next-dir "/") (assoc state :path ["/"])
                      (= next-dir "..") (update state :path pop)
                      :else (update state :path conj next-dir)))
                      ; :else (update state :path conj path)))
      (= op "ls") (let [output (rest (str/split command #"\n"))]
                    (assoc-in
                     state
                     [:system path]
                     {:sizes (map #(Integer/parseInt %) (re-seq #"\d+" (str output)))
                      :dirs (map second (re-seq #"dir (.*)" (str/join "\n" output)))})))))

(apply-command {:system {} :path ["/"]} "cd a") ; {:system {}, :path ["/" "a"]}
(apply-command {:system {} :path ["/" "a"]} "cd ..") ; ["/" "a"]
(apply-command {:system {} :path ["/"]} (second (split-program-to-commands demo-str)))
; {:system {["/"] {:sizes (14848514 8504156), :dirs ("a" "d")}},
;  :path ["/"]}

(def demo-system (reduce apply-command init-state (split-program-to-commands demo-str)))
; {:system
;  {["/"] {:sizes (14848514 8504156), :dirs ("a" "d")},
;   ["/" "a"] {:sizes (29116 2557 62596), :dirs ("e")},
;   ["/" "a" "e"] {:sizes (584), :dirs ()},
;   ["/" "d"] {:sizes (4060174 8033020 5626152 7214296), :dirs ()}},
;  :path ["/" "d"]}
(def input-system (reduce apply-command init-state (split-program-to-commands input-str)))

(defn system->directory-sizes
  "Gets directory to size mapping from a system"
  [system] (reduce
            (fn [dir->size dir] (let [children-sizes (map #(dir->size (conj dir %)) (get-in system [dir :dirs]))
                                      file-sizes (get-in system [dir :sizes])]
                                  (assoc dir->size dir (+ (apply + children-sizes) (apply + file-sizes)))))
            {}
            (reverse (sort (map first system))))) ; abuse the fact that by ordering path length we can compute from 'leaves'

(system->directory-sizes (:system demo-system)) ; {["/" "a" "e"] 584, ["/" "d"] 24933642, ["/" "a"] 94853, ["/"] 48381165}

(apply + (map second (filter #(< (second %) 100000) (system->directory-sizes (:system demo-system))))) ; 95437
(println "Part 1:" (apply + (map second (filter #(< (second %) 100000) (system->directory-sizes (:system input-system)))))) ; 1084134

; Part 2
; Find smallest directory that, when deleted, would free up enough space
; Total space 70000000, necessary space 30000000
; So .... find smallest directory that, upon deleting, results in total space of 40000000 or less

(let [directory->sizes (system->directory-sizes (:system demo-system))
     initial-size (get directory->sizes["/"])]
  (first (sort-by second (filter #(>= 40000000 (- initial-size (second %))) directory->sizes))))

(println "Part 2" (let [directory->sizes (system->directory-sizes (:system input-system))
     initial-size (get directory->sizes["/"])]
  (first (sort-by second (filter #(>= 40000000 (- initial-size (second %))) directory->sizes))))) ; [["/" "nns" "ncvv" "nnch"] 6183184]

