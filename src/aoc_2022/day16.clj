(ns aoc-2022.day16
  (:require
   [aoc-2022.utils :as utils]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def demo-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")
(def puzzle-input (->> "input16.txt" io/resource io/reader slurp))

; Parsing: a map from :valve -> {:flow [valve]}

:input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
:output [:AA {:rate 0 :connections [:DD :II :BB]}]
(defn parse-line
  "Parses a single serialized string representaiton to data"
  [line]
  (let [[_                  label         rate            connections]
        (re-matches #"Valve (\w+).* rate=(\d+).* valve(?:s*) (.*)" line)
        connections (read-string (str "[" connections "]"))]
    [(keyword label) {:rate (Integer/parseInt rate) :connections (mapv keyword connections)}]))
(parse-line "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB") ; [:AA {:rate 0, :connections [:DD :II :BB]}]
(parse-line "Valve JJ has flow rate=21; tunnel leads to valve II")         ; [:JJ {:rate 21, :connections [:II]}]

(def demo-pipe-system (into {} (map parse-line (str/split-lines demo-input))))
(def puzzle-pipe-system (into {} (map parse-line (str/split-lines puzzle-input))))

; Let us consider a way of enumerating over all possibilities.
; Instead of considering different moves, let us consider the order in which
; we can open the valves. We can then compute the quickest path, and from that
; compute the workflow
;
; So we first need to generate all permutations,
; which already sounds like a bad idea.
; So let's not, no permutations or considering all steps.
; Instead, we will consider 'open next valve' a _step_.
; And directly do search over that...?

; Given this search problem, let us define the important bits and pieces:
;   - Termination function (reached some depth?)
;   - Expansion function  (all next states possible from opening a new valve?)
;   - Evaluation function (outcome of our sequence)
;
; Let us first consider the state:
; We need to know where we are, and... which valves were opened at which time?
; Oh, and the current timestep!
; s = {:loc valve, :t 0, :opened-valves {valve1 t1, valve2 t2 ...}}

:input demo-pipe-system
:output {:loc :AA, :t 0, :opened-valves {:AA 0, :GG 0, :FF 0, :II 0}}
(defn pipes->initial-state [pipes]
  {:loc :AA, :t 0,
   :opened-valves (->> pipes
                       (filter #(zero? (:rate (second %))))  ; get pipes with 0 rate
                       (map #(vector (first %) 0))           ; ([valve1 0] [valve2 0] ...)
                       (into {}))})                          ; {valve1 0, valve2 0 ....}
(pipes->initial-state demo-pipe-system) ; {:loc :AA, :t 0, :opened-valves {:AA 0, :GG 0, :FF 0, :II 0}}

; Now that we have a state, let us first define a terminal predicate.
; Just based on current time step...

:input [demo-pipe-system 3 {:t 4}]
:output true
(defn terminates?
  "Returns true if a particular depth is reached or all valves are opened"
  [pipes max-time {:keys [t opened-valves]}]
  (or (<= max-time t)
      (= (count pipes) (count opened-valves))))
(terminates? demo-pipe-system 3 {:t 3}) ; true
(terminates? demo-pipe-system 3 {:t 2 :opened-valves #{}}) ; false
(terminates? demo-pipe-system 3 {:t 2 :opened-valves (into {} (for [l [:AA :BB :CC :EE :FF :GG :HH :JJ :II :DD]] [l 0]))}) ; true

; Then let us define the evaluation function.
; This should simply compute the full flow rate.
; Since we have a mapping valve -> time-step most of this can be computed easily.
; I just want to give it an additional max-depth, to be able to create until any depth.
; (This will allow us to stop the search when no more valves are to be opened!)

:input demo-pipe-system 5 {:t 4 :opened-valves {:BB 1 :JJ 2 :CC 5}}
:output '(+ (* 13 4) (* 21 3)) ; 115
(defn evaluate-state [pipes T {valves :opened-valves}]
  (->> valves
       (filter #(< (second %) T))                 ; filter out 'too late' (?) valves
       (map #(* (get-in pipes [(first %) :rate])  ; get accumulate flow of each valve
                (- T (second %))))
       (apply +)))
(def evaluate-state-memoized (memoize evaluate-state))
(evaluate-state demo-pipe-system 5 {:t 4 :opened-valves {:BB 1 :JJ 2 :CC 5}}) ; 115
(evaluate-state demo-pipe-system 5 {:t 4 :opened-valves {:BB 1 :JJ 2 :CC 4}}) ; 117
(evaluate-state-memoized demo-pipe-system 5 {:t 4 :opened-valves {:BB 1 :JJ 2 :CC 4}}) ; 117

; Lastly, the expansion function.
; This function should take in one of those states,
; And return all possible next states
; For this we need a function that returns the path length between two valves.

:input demo-pipe-system :AA :CC
:output 2
(defn path-length [pipes l l']
  (let [next-locations #(get-in pipes [% :connections])
        shortest-path (utils/bfs next-locations #{l'} l)]
    (loop [{n' :parent} shortest-path
           length 0]
      (if (nil? n')
        length
        (recur n' (inc length))))))
(path-length demo-pipe-system :AA :CC) ; 2

; Then the actual expand function should grab all un-opened valves.
; Get their path length.
; And create new states with new time, location, and opened valves.

:input [demo-pipe-system {:t 3, :loc :EE, :opened-valves (into {} (for [l [:AA :BB :CC :EE :FF :GG :HH :JJ]] [l 0]))}]
:output '({:t 4 :loc :DD :opened-valves {:AA 0 ... ... :DD 4}} ...)
(defn expand
  [pipes {:keys [t loc opened-valves]}]
  (->> (set/difference                                    ; All un-opened valves.
        (set (keys pipes))
        (set (map first opened-valves)))
       (map #(vector % (+ t (inc (path-length pipes loc %)))))  ; ([valve t] ...)  ; inc to 'go to path' + open
       (map #(hash-map                                    ; ({:t :loc :opened-valves} ...)
              :t (second %),
              :loc (first %),
              :opened-valves (conj opened-valves %)))))
(expand demo-pipe-system {:t 3, :loc :EE, :opened-valves (into {} (for [l [:AA :BB :CC :EE :FF :GG :HH :JJ]] [l 0]))})

:input [demo-pipe-system 30]
:output [1651 {:t 24, :loc :CC, :opened-valves {:CC 24}}]
(defn part-1 [pipes max-depth]
  (utils/exhaustive-search-state
   #(expand pipes %)
   #(evaluate-state pipes max-depth %)
   #(terminates? pipes max-depth %)
   (pipes->initial-state pipes)))

(time (second (part-1 puzzle-pipe-system 22)))

(println "Part 1:" (second (part-1 puzzle-pipe-system 30)))

; Optimization for fun
; So I think the evaluation is a pain, and we are re-calculating a lot.
; In particular, whenever we open a valve, we can actually know its affect on the total flow.
; So during the expand, given the 'max time', we can maintain what the value would be.
;
; We need to change a lot for this:
;   - maintain set of opened valves (not mapping to time).
;   - set :total-flow in initial state.
;   - update :total-flow in expand.
;   - give back this total whenever evaluate is used.

:input demo-pipe-system
:output {:loc :AA, :t 0, :total-flow 0, :opened-valves #{:AA :GG :FF :II}}
(defn efficient-pipes->initial-state [pipes]
  {:loc :AA, :t 0, :total-flow 0
   :opened-valves (->> pipes                                 ; {:valve {:rate 1, :connections [:valve1 :valve2 ...]} ...}
                       (filter #(zero? (:rate (second %))))  ; filter for those with :rate 0
                       (map first)                           ; (:valve ...)
                       set)})
(efficient-pipes->initial-state demo-pipe-system) ; {:loc :AA, :t 0, :total-flow 0, :opened-valves #{:AA :GG :FF :II}}

:input [demo-pipe-system 5 {:t 3, :loc :EE, :total-flow 10, :opened-valves #{:AA :BB :CC :EE :FF :GG :HH :JJ}}]
:output '({:t 5, :loc :DD, :total-flow 30 :opened-valves #{:AA ... :DD}} ...)
(defn efficient-expand
  [pipes max-t {:keys [t loc total-flow opened-valves]}]
  (->> opened-valves
       (set/difference (set (keys pipes)))                       ; Get all maintaining valves
       (map #(vector % (+ t (inc (path-length pipes loc %)))))   ; ([valve t] ...)  ; inc to 'go to path' + open
       (map (fn [[v t]] {:t t,                                   ; ({:t :loc :total-flow :opened-valves} ...)
                         :loc v,
                         :total-flow (+ total-flow (* (get-in pipes [v :rate]) (max 0 (- max-t t)))),
                         :opened-valves (conj opened-valves v)}))))
(efficient-expand demo-pipe-system 4 {:t 3, :loc :EE, :total-flow 10, :opened-valves #{:AA :BB :CC :EE :FF :GG :HH :JJ}})

:input {:t 4 :total-flow 123 :opened-valves {:BB 1 :JJ 2 :CC 5}}
:output 123
(defn efficient-evaluate-state [{val :total-flow}] val)
(efficient-evaluate-state {:t 4 :total-flow 123 :opened-valves {:BB 1 :JJ 2 :CC 5}}) ; 123

(time (second (let [pipes demo-pipe-system
                    T 30]
                (utils/exhaustive-search
                 #(efficient-expand pipes T %)
                 efficient-evaluate-state
                 #(terminates? pipes T %)
                 (efficient-pipes->initial-state pipes))))) ; 1651

(time (second (let [pipes puzzle-pipe-system
                    T 30]
                (utils/exhaustive-search
                 #(efficient-expand pipes T %)
                 efficient-evaluate-state
                 #(terminates? pipes T %)
                 (efficient-pipes->initial-state pipes))))) ; 1474

; Part 2

; We now teach elephants to help me out.
; This means 2 'actions' per step, but only 26 steps.
; I have to reconsider _everything_.
; I probably did something wrong in the first place anyways...

; We are going to do another search.
; The 'expansion' is picking the next valve to open.
; However, this can be either the elephant or me doing the opening.
; The next state will depend on who gets to the first valve...

; The state should include :loc-elephant
; Otherwise, we need to keep track of who is doing what.

:input demo-pipe-system
:output {:loc :AA, :loc-elephant :AA, :t 0, :opened-valves {:AA 0, :GG 0, :FF 0, :II 0}}
(defn efficient-pipes->initial-state-2 [pipes]
  (assoc (efficient-pipes->initial-state pipes) :loc-elephant :AA))                          ; {valve1 0, valve2 0 ....}
(efficient-pipes->initial-state-2 demo-pipe-system)
; {:loc :AA,
;  :t 0,
;  :total-flow 0,
;  :opened-valves #{:AA :GG :FF :II},
;  :loc-elephant :AA}

; So a "step" now means setting goals and "traversing".
; The idea is that there are 'two' steps: 
;   1. Assign goal if not given
;   2. If goals assigned, apply steps until first goal is reached.

(defn expand-2
  "Gives a next state for part 2"
  [pipes T {:keys [t total-flow opened-valves loc loc-elephant next-valve next-valve-elephant] :as s}]
  (cond
    ; Forward to nearest goal!
    (and next-valve next-valve-elephant) (let [[l dissoc-k] (if (<= (:t next-valve) (:t next-valve-elephant)) [:loc :next-valve] [:loc-elephant :next-valve-elephant])
                                               {:keys [t loc flow]} (if (= l :loc) next-valve next-valve-elephant)]
                                           [(-> s
                                                (assoc :t t, l loc, :total-flow (+ total-flow flow))
                                                (dissoc dissoc-k))])
    ; Weird corner case: one last 'step' to go!
    (= (count pipes) (count opened-valves)) (let [[l dissoc-k] (if next-valve [:loc :next-valve] [:loc-elephant :next-valve-elephant])
                                                  {:keys [t loc flow]} (if next-valve next-valve next-valve-elephant)]
                                              [(-> s
                                                   (assoc :t t, l loc, :total-flow (+ total-flow flow))
                                                   (dissoc dissoc-k))])
    ; Either we or the elephant needs a next goal
    :else (let [[l' k] (if next-valve [loc-elephant :next-valve-elephant] [loc :next-valve])
                next-valves (->> opened-valves
                                 (set/difference (set (keys pipes)))
                                 (map #(vector % (+ t (inc (path-length pipes l' %))))))]
            (map
             (fn [[v t]] (assoc s k {:t t, :loc v, :flow (* (get-in pipes [v :rate]) (max 0 (- T t)))},
                                :opened-valves (conj opened-valves v)))
             next-valves))))

(defn terminates-2?
  "True if neither we nor the elephant have any goals and all pipes are opened"
  [pipes max-time {:keys [t opened-valves next-valve next-valve-elephant]}]
  (or (<= max-time t)
      (and (= (count pipes) (count opened-valves))
           (and (nil? next-valve) (nil? next-valve-elephant)))))

(time (second (let [pipes puzzle-pipe-system, T 26]
                (utils/exhaustive-search-state
                 #(expand-2 pipes T %)
                 efficient-evaluate-state
                 #(terminates-2? pipes T %)
                 (efficient-pipes->initial-state-2 pipes)))))
