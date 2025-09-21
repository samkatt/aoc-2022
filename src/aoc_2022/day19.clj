(ns aoc-2022.day19)

(def demo-input "Blueprint 1:
Each ore robot costs 4 ore.
Each clay robot costs 2 ore.
Each obsidian robot costs 3 ore and 14 clay.
Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
Each ore robot costs 2 ore.
Each clay robot costs 3 ore.
Each obsidian robot costs 3 ore and 8 clay.
Each geode robot costs 3 ore and 12 obsidian")
(def demo-initial-state {:blue-print [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]] :t 0 :resources [0 0 0 0] :robots [1 0 0 0]})
(def demo-blue-prints '([[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]] [[2 0 0 0] [3 0 0 0] [3 8 0 0] [3 0 12 0]]))
(def demo-policy {0 1
                  3 1
                  5 1
                  7 2
                  11 1
                  12 2
                  15 3
                  18 3
                  21 3
                  24 3})

; math utilities
(defn v+
  [v1 v2] (mapv + v1 v2))
(defn v-
  [v1 v2] (mapv - v1 v2))
(v+ [0 1 0 1] [1 2 3 4]) ; (1 3 3 5)
(v- [0 1 0 1] [1 2 3 4]) ; (-1 -1 -3 -3)

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

; Moves / actions that affect the state
(defn can-build-robot? [{:keys [blue-print resources]} robot]
  (v<= (nth blue-print robot) resources))

(defn build-robot "Updates state by building a robot"
  [{:keys [blue-print resources robots] :as state} robot]
  (assoc state
         :resources (v- resources (nth blue-print robot))
         :robots (update robots robot inc)))
(build-robot {:blue-print [[2 0 0]] :resources [0 0 0] :robots [2 1 0]} 0) ; {:blue-print [[2 0 0]], :resources (-2 0 0), :robots [3 1 0]}

(defn mine-resources
  [{:keys [robots resources t] :as state}]
  (assoc state
         :t (inc t)
         :resources (v+ resources robots)))
(take 3 (iterate mine-resources {:robots [1 2 3 4] :resources [2 1 2 1] :t 10}))
; ({:robots [1 2 3 4], :resources [2 1 2 1], :t 10}
;  {:robots [1 2 3 4], :resources [3 3 5 5], :t 11}
;  {:robots [1 2 3 4], :resources [4 5 8 9], :t 12})

(defn step
  [{:keys [] :as state} robot-to-build] ; if robot-to-build is negative, it is assumed 'none'
  (if (neg? robot-to-build)
    (mine-resources state)
    (if (can-build-robot? state robot-to-build)
      (build-robot (mine-resources state) robot-to-build)
      (throw (RuntimeException. (str "Cannot build robot " robot-to-build " in " state))))))
; (step demo-initial-state 2) << throws error
(reduce step demo-initial-state [-1 -1 1 -1 1])
; {:blue-print [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]],
;  :t 5,
;  :resources [1 2 0 0],
;  :robots [1 2 0 0]}

(defn possible-actions [state]
  (into (list -1) (remove nil? (map-indexed #(when (can-build-robot? state %2) %1) (range 4)))))
(possible-actions (assoc demo-initial-state :resources [3 3 7 0])) ; (3 1 -1)

; part 1 naive: go over all policies and pick best
(defn rollout-policy
  [policy state]
  (let [next (step state (policy state))] (lazy-seq (cons next (rollout-policy policy next)))))

(defn score-path [path]
  (last (:resources (last path))))
(score-path [123 23 demo-initial-state]) ; 0
(score-path [123 23 (assoc demo-initial-state :resources [23 43 12 -23])]) ; -23

(defn dfs [pred action-selector path]
  (let [state (last path)]
    (if (pred path)
      ; backing up
      path
      ; deeper!
      (apply max-key score-path
             (for [next-state (map (partial step state) (action-selector state))]
               (dfs pred action-selector (conj path next-state)))))))

(defn basic-dfs
  [initial-state depth-to-go]
  (dfs #(= depth-to-go (count %)) possible-actions [initial-state]))

(basic-dfs demo-initial-state 20)

; relatively useful ad-hoc hacks to make search better
(defn smart-action-selector
  [max-necessary-ore-robots {:keys [robots] :as state}]
  (cond
    (can-build-robot? state 3) [3]
    (can-build-robot? state 2) [2]
    :else (let [all-actions (possible-actions state)]
            (if (= max-necessary-ore-robots (first robots))
              (remove #{0} all-actions)
              all-actions))))

(map first (:blue-print demo-initial-state))

(time (dfs #(= 21 (count %)) possible-actions [demo-initial-state]))
; Elapsed time: 193375.754797 msecs
(time (dfs #(= 21 (count %)) smart-action-selector [demo-initial-state]))
; Elapsed time: 7284.152496 msecs
(time (dfs #(= 22 (count %)) smart-action-selector [demo-initial-state]))
; Elapsed time: 14452.598508 msecs
(time (dfs #(= 25 (count %)) smart-action-selector [demo-initial-state]))
; Elapsed time: 126806.915132 msecs

(time (dfs #(= 21 (count %)) (partial smart-action-selector 2) [demo-initial-state]))
; Elapsed time: 333.667876 msecs
(time (dfs #(= 22 (count %)) (partial smart-action-selector 2) [demo-initial-state]))
; Elapsed time: 471.31785 msecs
(def demo-path (dfs #(= 25 (count %)) (partial smart-action-selector 2) [demo-initial-state]))
; Elapsed time: 1166.234717 msecs

(defn smart-dfs
  "Runs my smartest DFS so far"
  [depth-to-go initial-state]
  (dfs #(= depth-to-go (count %)) (partial smart-action-selector 3) [initial-state]))
(last (smart-dfs 20 demo-initial-state))
; {:blue-print [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]],
;  :t 19,
;  :resources [3 20 4 1],
;  :robots [1 4 2 1]}

(def tmp (map (partial smart-dfs 25) (map #(assoc {:t 0 :resources [0 0 0 0] :robots [1 0 0 0]} :blue-print %) demo-blue-prints)))

(def tmp-2 (smart-dfs 25 (assoc {:t 0 :resources [0 0 0 0] :robots [1 0 0 0]} :blue-print (second demo-blue-prints))))



