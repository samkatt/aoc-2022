(ns aoc-2022.utils)

(defn v+
  [v1 v2] (mapv + v1 v2))
(defn v-
  [v1 v2] (mapv - v1 v2))
(v+ [0 1 0 1] [1 2 3 4]) ; [1 3 3 5]
(v- [0 1 0 1] [1 2 3 4]) ; [-1 -1 -3 -3]

(defn v<
  [v1 v2] (every? boolean (map < v1 v2)))
(defn v<=
  [v1 v2] (every? boolean (map <= v1 v2)))
(defn v>
  [v1 v2] (not (v< v1 v2)))
(defn v>=
  [v1 v2] (not (v<= v1 v2)))
(v< [0 1 2 4] [1 2 3 4]) ; false
(v< [0 1 2 3] [1 2 3 4]) ; true
(v> [0 1 2 3] [1 2 3 4]) ; false

(defn manhattan
  "Returns manhatten distance between two points"
  [[y1 x1] [y2 x2]] (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn exhaustive-search
  "A general, exhaustive, search algorithm.
  Will call `f-expand` on state `s` to generate next states.
  Then for each of these, add it to the current history `h`.
  Then recursively call itself for each new history.
  If `terminates?` returns true for a history,
  this will `f-eval` the history and return it (plus the sequence).
  - `f-expand`    : s -> (s1 s2 ...)
  - `f-eval`      : s -> num
  - `terminates?` : s -> bool
  "
  [f-expand f-eval terminates? s]
  (if (terminates? s)
    [s (f-eval s)]
    (->> s
         f-expand                                           ; (s1 s2 ...)
         (map #(exhaustive-search f-expand f-eval terminates? %))  ; ([v1 s1'] [v2 s2'] ...)
         (apply max-key second))))

(defn exhaustive-search-rec
  "A general, exhaustive, search algorithm.
  Will call `f-expand` on state `s` to generate next states.
  Then for each of these, add it to the current history `h`.
  Then recursively call itself for each new history.
  If `terminates?` returns true for a history,
  this will `f-eval` the history and return it (plus the sequence).

  - `f-expand`    : s -> (s1 s2 ...)
  - `f-eval`      : s -> num
  - `terminates?` : s -> bool
  - `s`           : initial state s

  Note this is the recursive version of `max-search`
  "
  ([f-expand f-eval terminates? s] (exhaustive-search-rec f-expand f-eval terminates? [nil ##-Inf] [s]))
  ([f-expand f-eval terminates? best [s & queue]]
   ; There are three options:
   (cond
     ; We are done searching -> return best
     (nil? s) best
     ; We have reached an end -> return best
     (terminates? s) (let [v (f-eval s)
                           best' (if (> (second best) v) best [s v])]
                       (recur f-expand f-eval terminates? best' queue))
     ; Else we continue search -> expand and recur with new queue
     :else (let [expansions (f-expand s)]
             (recur f-expand f-eval terminates? best (concat expansions queue))))))

(defn exhaustive-search-state
  "A general, exhaustive, search algorithm.
  
  Will call `f-expand` on state `s` to generate next states.
  Then for each of these, add it to the current history `h`.
  Then recursively call itself for each new history.
  If `terminates?` returns true for a history,
  this will `f-eval` the history and return it (plus the sequence).

  - `f-expand`    : s -> (s1 s2 ...)
  - `f-eval`      : s -> num
  - `terminates?` : s -> bool
  - `s`           : initial state s

  Note, this is the state-ful implementation of `max-search`
  "
  ([f-expand f-eval terminates? s]
   (let [queue (atom (list s))
         pop-queue (fn [] (let [s (peek @queue)] (swap! queue pop) s))
         add-to-queu #(swap! queue into %)]
     (loop [best [nil ##-Inf]]
       (if (empty? @queue)
         best
         (let [s (pop-queue)]
           (if (terminates? s)
             ; Continue with next best
             (let [v (f-eval s)]
               (if (> (second best) v)
                 (recur best)
                 (recur [s v])))
             ; Expand and continue by adding to queue
             (do
               (add-to-queu (f-expand s))
               (recur best)))))))))

(defn bfs "Runs breadth-first-search.
  Expects an initial 'state' `s` and uses `f-expand` to generate next states.
  Will stop when `goal?` predicate on `s` returns true."
  ([f-expand goal? s]
   (bfs f-expand goal? #{s} (for [s' (f-expand s)] {:s s' :parent {:s s :parent nil}})))
  ([f-expand goal? visited [{s :s :as n} & tail]]
   (cond
     ; Return head if it is the goal or nil.
     (or (goal? s) (nil? s)) n
     ; Continue with tail if already visited.
     (visited s) (recur f-expand goal? visited tail)
     ; Default: expand to tail and continue.
     :else (let [next-nodes
                 (for [s' (f-expand s)] {:s s' :parent n})]
             (recur f-expand goal? (conj visited s) (concat tail next-nodes))))))
