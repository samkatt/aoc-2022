(ns aoc-2022.day12
  (:require
   [aoc-2022.utils :refer [manhattan]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

; some processing/storing of input
(def demo-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")
(def puzzle-input (slurp (io/reader (io/resource "input12.txt"))))

(defn char->height
  "Returns the height encoding of a character"
  [c] (if (Character/isLowerCase c)
        (- (int c) (int \a))
        (cond
          (= \S c) (char->height \a)
          (= \E c) (char->height \z))))
(char->height \b) ; 1
(char->height \S) ; 0
(char->height \E) ; 25
(char->height \z) ; 25

(defn- coordinate-of-character
  "Finds the location of a character (very inefficient lookup in string) "
  [str char]
  (reduce
   (fn [[y x] c]
     (if (= char c)
       (reduced [y x])
       (if (= \newline c)
         [(inc y) 0]
         [y (inc x)])))
   [0 0]
   str))
(coordinate-of-character demo-input \E) ; [2 5]
(coordinate-of-character puzzle-input \S) ; [20 0]

(defn input->initial-state
  "Takes input (str) description and returns {:map :loc :goal}"
  [str] {:depth-map (mapv (partial mapv char->height) (str/split str #"\n"))
         :loc (coordinate-of-character str \S)
         :goal (coordinate-of-character str \E)})
(def demo-initial-state (input->initial-state demo-input))
; {:map
;  [[0 0 1 16 15 14 13 12]
;   [0 1 2 17 24 23 23 11]
;   [0 2 2 18 25 25 23 10]
;   [0 2 2 19 20 21 22 9]
;   [0 1 3 4 5 6 7 8]],
;  :loc [0 0],
;  :goal [2 5]}
(def puzzle-initial-state (input->initial-state puzzle-input))

; So now we have the depth map, initial location and goal.
; We are supposed to find the shortest path from `:loc` to `:goal`
; My initial, naive plan, is to do a DFS with some early breaking

; We make the design decision that a 'state' in the search contains
;   - the current location
;   - a set of visited locations
; {:loc :visited}

; We assume (and provide) 3 type of functions:
;   - a predicate that tells us when to stop the search
;   - an expansion function that gives us next 'states' to consider
;   - a scoring method to order 'states'

(defn break-search?
  "Tells us when to stop a search. Generally either
  (1) when a goal is reached or
  (2) when we know it is worse than the current best
  Right now we are just returning if a goal is found"
  [{:keys [goal]} {:keys [loc]}] (= loc goal))
(break-search? {:goal [4 4]} {:loc [4 4]}) ; true
(break-search? {:goal [3 2]} {:loc [2 1]}) ; false

(defn path-score
  "Provides a 'score', the evaluation, of a path.
  In this particular case, we care only about the lenght of the path
  Returns a large negative number if the goal is not reached"
  [{:keys [goal]} {:keys [visited loc]}]
  (if (= loc goal)
    (- (inc (count visited)))
    -1000))
(path-score {:goal [3 3]} {:visited #{[0 1] [0 2] [0 3] [1 3]} :loc [2 3]}) ; -1000
(path-score {:goal [2 3]} {:visited #{[0 1] [0 2] [0 3] [1 3]} :loc [2 3]}) ; -5

(defn within-bounds?
  "Returns whether or not a location is within bounds"
  [depth-map [y x]] (let [y-max (count depth-map)
                          x-max (count (first depth-map))]
                      (and (<= 0 y) (<= 0 x) (< x x-max) (< y y-max))))
(within-bounds? (:depth-map demo-initial-state) [0 0]) ; true
(within-bounds? (:depth-map demo-initial-state) [0 -1]) ; false
(within-bounds? (:depth-map demo-initial-state) [4 7]) ; true
(within-bounds? (:depth-map demo-initial-state) [4 8]) ; false
(within-bounds? (:depth-map demo-initial-state) [5 7]) ; false

(defn can-traverse?
  "Returns true if `loc'` can be traversed to from `loc` given the `depth-map`"
  [depth-map loc loc'] (> 2 (- (get-in depth-map loc') (get-in depth-map loc))))
(can-traverse? (:depth-map demo-initial-state) [0 0] [0 1]) ; true
(can-traverse? (:depth-map demo-initial-state) [0 1] [1 1]) ; true
(can-traverse? (:depth-map demo-initial-state) [1 1] [1 2]) ; true
(can-traverse? (:depth-map demo-initial-state) [1 2] [1 3]) ; false
(can-traverse? (:depth-map demo-initial-state) [2 0] [2 1]) ; false
(can-traverse? (:depth-map demo-initial-state) [0 3] [0 2]) ; true
(can-traverse? (:depth-map demo-initial-state) [0 2] [0 3]) ; false

(defn expand-search
  "Gives us next 'states' to consider
  Remember, each 'state' is {:loc :visited}
  Note that next states are:
    - within bounds
    - either up/left/down/right of current location
    - not already visited"
  [{:keys [depth-map]} {:keys [loc visited]}]
  (let [[y x] loc
        next-locations [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]]
    (map #(assoc {} :loc % :visited (conj visited loc))
         (filter #(and
                   (not (contains? visited %))
                   (within-bounds? depth-map %)
                   (can-traverse? depth-map loc %))
                 next-locations))))
(expand-search demo-initial-state {:loc [0 3] :visited #{[0 4]}})
; ({:loc [1 3], :visited #{[0 3] [0 4]}}
;  {:loc [0 2], :visited #{[0 3] [0 4]}})

(defn dfs
  "Naive depth-first search to go from some location to the goal
  Search depends on the (break/prune) `predicate`, `expand` function and `path-score`
  Then we are given the (static) global problem, maintain the current best solution and current state"
  [f-expand problem best-solution state]
  (if (or (break-search? problem state)
          (and (some? best-solution) (>= (count (:visited state)) (count (:visited best-solution)))))
   ; Base-case: pruning this search (just give back current path)
    state
    ; Continue search: take best of expansions
    (let [expansions (expand-search problem state)]
      (if (empty? expansions)
        state
        (apply max-key (partial path-score problem)
               (for [state' expansions]
                 (dfs f-expand problem best-solution state')))))))

(time (def demo-solution (dfs expand-search demo-initial-state nil {:visited #{} :loc (:loc demo-initial-state)})))
(count (:visited demo-solution)) ; 31

; Unfortunately doing this solution on puzzle leads nowhere...
; (dfs expand-search puzzle-initial-state nil {:visited #{} :loc (:loc puzzle-initial-state)})

; I can see three paths forward:
;   (1) re-write `dfs` to not be recursive (and not have stack overflow)
;   (2) add a heuristic (sorting) in expansions and hopefully avoid really long paths
;   (3) use a different algorithm (A*)

; Since (2) is easiest, I'll try that first

(defn expand-search-with-smart-sorting
  "Gives next 'states' to consider given a search, much like `expand-search`.
 Main difference is that here we _sort_ them according to an heuristic:
  - prefer going 'up'
  - prefer going 'towards the goal'"
  [{:keys [depth-map goal] :as problem}
   state]
  (sort-by (juxt
            #(- (get-in depth-map (:loc %)))
            #(manhattan goal (:loc %)))
           (expand-search problem state)))
(expand-search-with-smart-sorting demo-initial-state {:loc [1 3] :visited #{}})
; ({:loc [2 3], :visited #{[1 3]}}
;  {:loc [0 3], :visited #{[1 3]}}
;  {:loc [1 2], :visited #{[1 3]}})

(time (count (:visited (dfs expand-search-with-smart-sorting demo-initial-state nil {:visited #{} :loc (:loc demo-initial-state)}))))

; The easiest approach, improving the search, still leads nowhere
; (def puzzle-solution (dfs expand-search-with-smart-sorting puzzle-initial-state nil {:visited #{} :loc (:loc puzzle-initial-state)}))

; So I'm going to try implement it with either tail-recursion or without recursion at all
; Initially, the idea is to maintain a (FILO?) queue, to maintain the branches explicitly (instead of in control flow)

; The 'queue', since we are doing DFS, will be FILO, and I think just a simple list
; We would like to have the 'nodes' in the list to have the following:
;   - reference to their parents (to get the path back?)
;   - enough state to be able to expand (just location?)
;   - a way to quickly get path length and check if walking a circle (set of visited locations?)

; After some experimenting, figured out that if we maintain a list of `loc -> earliest-time`, then
; we can prune any path that reaches a location later

; Hence I think a node is just {:loc :parent :lenght}
; Otherwise we just need to maintain a set of all visited locations! (In addition to the current best path)

(defn expand-location
  "Simple function that provides next locations,
  similar to `expand-search`. Just returns 'possible' next locations"
  [depth-map [y x]] (filter
                     #(and (within-bounds? depth-map %) (can-traverse? depth-map [y x] %))
                     [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]))
(expand-location (:depth-map demo-initial-state) [1 2]) ; ([2 2] [0 2] [1 1])

(defn expand
  "Uses `expand-location` to do proper expansion in `dfs-tail-recursion`.
  Basically makes sure the proper 'nodes' are created.
  Note this _does not check_ whether a place has been visited before"
  [depth-map {:keys [loc length] :as node}]
  (map
   #(assoc {} :parent node :loc % :length (inc length))
   (expand-location depth-map loc)))
(expand (:depth-map demo-initial-state) {:parent nil :loc [1 3] :length 1})
; ({:parent {:parent nil, :loc [1 3], :length 1}, :loc [2 3], :length 2}
;  {:parent {:parent nil, :loc [1 3], :length 1}, :loc [0 3], :length 2}
;  {:parent {:parent nil, :loc [1 3], :length 1}, :loc [1 2], :length 2})

(defn in-path?
  "Returns whether a location has been visited in current path
  Used for debugging (slow way of checking circular paths)"
  [{:keys [parent loc]} loc-to-test]
  (if (nil? loc)
    false
    (or (= loc loc-to-test) (recur parent loc-to-test))))

(defn dfs-tail-recursion
  "DFS with tail recursion!
  Maintains a queue to figure out how to continue branching.
  This search _starts_ with creating the queue (first parity)
  "
  ([{:keys [depth-map loc] :as problem}]
   (dfs-tail-recursion problem nil {loc 0} (expand depth-map {:parent nil :loc loc :length 0})))
  ([{:keys [goal] :as problem}
    best-node
    loc->depth
    [{:keys [length loc] :as head} & tail]]
   ; There are a few cases to consider
   (cond
     ; base case: nothing left to search!
     (nil? head) best-node
     ; current best solution is shorter: recur with rest of queue
     (and (some? best-node) (< (:length best-node) length)) (recur problem best-node loc->depth tail)
     ; we found the goal: pick best and continue search
     (= goal loc) (recur problem (if (nil? best-node) head (min-key :length best-node head)) loc->depth tail)
     ; we have visited this location before in a quicker manner: ignore and continue with tail
     (and (contains? loc->depth loc) (>= length (loc->depth loc))) (recur problem best-node loc->depth tail)
     ; None of the above apply: simply continue searching
     :else (recur problem best-node (assoc loc->depth loc length) (concat (expand (:depth-map problem) head) tail)))))

(time (dfs-tail-recursion demo-initial-state))

; And this seems to work!
(def puzzle-solution (dfs-tail-recursion puzzle-initial-state))
(println "Part 1:" (:length puzzle-solution)) ; 447

; But I tried BFS too
; Similar to before, we maintain a list of nodes to extend
; A node will have be {:loc :parent :depth} (not sure if :length is necessary here)

; But we will maintain a set of locations, to prune the search

(defn bfs
  "Runs breadth-first-search.
  Will have two arities, one for 'starting' and one for recursive running"
  ([{:keys [depth-map loc] :as problem}]
   (bfs problem #{loc} (expand depth-map {:loc loc :parent nil :length 0})))
  ([{:keys [depth-map goal] :as problem} visited-locations [{:keys [loc] :as head} & tail]]
   ; There are several possibilities
   (cond
     (nil? head) nil
     ; we found the goal: done and return head
     (= goal loc) head
     ; this location has been visited before: continue with tail
     (contains? visited-locations loc) (recur problem visited-locations tail)
     ; else we just continue (important that we _append_ )
     :else (recur problem (conj visited-locations loc) (concat tail (expand depth-map head))))))

(time (:length (bfs demo-initial-state))) ; 31
(def bfs-part-1 (bfs puzzle-initial-state))
(:length bfs-part-1) ; 447

; Part 2
; Here we are interested in the quickest route to any 0 depth location
; We'll do this immediately with bfs

(defn bfs-part-2
  "Runs breadth-first-search on part 2 problem"
  ([goal-pred expand-f problem state]
   (bfs-part-2 goal-pred expand-f problem #{state} (expand-f problem {:state state :length 0 :parent nil})))
  ([goal-pred expand-f problem visited-states [{:keys [state] :as head} & tail]]
   ; We have different situations to consider
   (cond
     ; Base case: nothing left in queue
     (nil? head) nil
     ; Found goal: return
     (goal-pred problem state) head
     ; Visited state before: continue with tail
     (contains? visited-states state) (recur goal-pred expand-f problem visited-states tail)
     ; Else: store state and continue in BFS manner
     :else (recur goal-pred expand-f problem (conj visited-states state) (concat tail (expand-f problem head))))))

; Now let us implement the correct things to make `bfs-part-2` work:
(defn goal?
  "Returns whether, for part 2, the goal has been reached
  Basically just checks whether `loc` has depth 0"
  [{:keys [depth-map]} loc] (= 0 (get-in depth-map loc)))
(goal? demo-initial-state [0 0]) ; true
(goal? demo-initial-state [1 1]) ; false
(goal? demo-initial-state [4 0]) ; true

(defn expand-location-part-2
  "For part 2 we do a 'reverse' search.
  This means that we are expending reversely:
  Only locations 1 lower (or any higher) are allowed.
  For interested readers, we are copying `expand-location` but reversing input to `can-traverse?`"
  [depth-map [y x]] (filter
                     #(and (within-bounds? depth-map %) (can-traverse? depth-map % [y x]))
                     [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]))

(defn expand-bfs-part-2
  "The simple expansion function for searching in part 2.
  Simply expands all locations `expand-location` and wraps into node {:parent :length :state}"
  [problem {:keys [state length] :as node}]
  (map
   #(assoc {:parent node :length (inc length)} :state %)
   (expand-location-part-2 (:depth-map problem) state)))
(expand-bfs-part-2 demo-initial-state {:parent nil :length 1 :state [1 3]})
; ({:parent {:parent nil, :length 1, :state [1 3]},
;   :length 2,
;   :state [2 3]}
;  {:parent {:parent nil, :length 1, :state [1 3]},
;   :length 2,
;   :state [0 3]}
;  {:parent {:parent nil, :length 1, :state [1 3]},
;   :length 2,
;   :state [1 4]})

(time (:length (bfs-part-2 goal? expand-bfs-part-2 demo-initial-state (:goal demo-initial-state)))) ; 29

(def part-2-solution (bfs-part-2 goal? expand-bfs-part-2 puzzle-initial-state (:goal puzzle-initial-state)))
(println "Part 2:" (:length part-2-solution)) ; 446
