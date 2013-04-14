(ns ^{:author "kevin"}
     language-games.populations
  "Some basic implementations of the language-games.core.Population protocol"
  (:use [language-games.core]
        [language-games.utils.collections :only [rand-nths rand-nths-indexed]]))

(def production-perception-loop-behaviour
  "Default implementations for running games with a single agent in a
  production-perception loop. Associate by calling (make-production-perception-loop),
  or (extend <YourAgentType> Population production-perception-loop-behaviour) for
  extra customisation."
  {:size (constantly 1)
   :agents list
   :draw-pair #(vector 0 % 0 %)
   :update-agents (fn [this outcome] (update this :listener outcome))
   :advance-population identity})

(defn make-production-perception-loop
  "Makes instances of the given type/record/class (which should implement the
  Agent protocol) amenable to be passed to (run-game) as a population, resulting
  in the agent being put into a production-perception loop."
  [type]
  (extend type Population production-perception-loop-behaviour))

(def seqable-behaviour
  "Default implementations for running language games on seqable collection of agents which
   are simply treated as a fully connected population. Does not implement timekeeping/iteration
   count tracking."
  {:iteration (fn [_] nil)
   :size count
   :agents identity
   :draw-pair (fn [this] (rand-nths-indexed this 2)) ; fully connected
   ; update speaker and listener, agent models can choose not to react depending on their role
   :update-agents (fn [this {:keys [i j] :as outcome}]
                    (-> this
                      (update-in [i] #(update % :speaker outcome))
                      (update-in [j] #(update % :listener outcome))))
   :advance-population identity}); no timekeeping/population update

(defn make-agent-vector
  "Checks a given vector of agents for appropriate length or otherwise
   generates a vector of agents based on the agent-generation function"
  [n agent-vec-or-fn]
  {:pre [(or (fn? agent-vec-or-fn)
             (and (vector? agent-vec-or-fn) (= n (count agent-vec-or-fn))))]}
  (if (fn? agent-vec-or-fn)
    (vec (repeatedly n agent-vec-or-fn))
    agent-vec-or-fn))

; we can use simple vectors to represent fully connected populations without turnover
(extend clojure.lang.PersistentVector
  Population
  seqable-behaviour)
(defn new-closed-population
  "Creates a new fully connected population with n agents initialised by calling new-agent-fn."
  [n agent-vec-or-fn]
  (make-agent-vector n agent-vec-or-fn))

(def timekeeping-behaviour
  "Default implementations for a few methods that works for defrecords
   defining the two fields cnt (an int counter) and agts (a seq of agents)"
  {:iteration (fn [this] (:cnt this))
   :size (fn [this] (count (:agts this)))
   :agents (fn [this] (:agts this))
   :draw-pair (fn [this] (rand-nths-indexed (:agts this) 2))
   :update-agents (fn [this {:keys [i j] :as outcome}]
                    (-> this
                      ; could pass current iteration/time to agents too?
                      (update-in [:agts i] #(update % :speaker outcome))
                      (update-in [:agts j] #(update % :listener outcome))))
   ; default implementation which only updates the iteration counter
   :advance-population (fn [this n] (update-in this [:cnt] #(+ n %)))})

; open populations with turnover/replacement, but a constant number of agents.
; new-agent-fn is the function with which to create new agents
; turnover is the number of interactions before the oldest agent is replaced
; oldest holds the index of the currently oldest agent
; cnt counts the number of interactions until it reaches turnover and is reset
; agts is a vector (or map) of agents
(defrecord OpenPopulation [new-agent-fn turnover oldest cnt agts])
(extend OpenPopulation
  Population
  (assoc timekeeping-behaviour
         ; this function can only handle one replacement per
         ; call i.e. n must not be greater than turnover
         :advance-population (fn [this n]
                               (let [newcnt (+ (:cnt this) n)]
                                 (if (>= newcnt (:turnover this))
                                   (-> this
                                     (assoc :cnt (- newcnt (:turnover this)))
                                     (assoc-in [:agts (:oldest this)] ((:new-agent-fn this)))
                                     (assoc :oldest (mod (inc (:oldest this)) (size this))))
                                   ; else: just increment the counter
                                   (assoc this :cnt newcnt))))))

(defn new-open-population [n new-agent-fn turnover]
  {:pre [(fn? new-agent-fn)]}
  (OpenPopulation. new-agent-fn turnover 0 0 (vec (repeatedly n new-agent-fn))))

; unweighted (i.e. simple connected/unconnected) population
(defrecord StructuredPopulation [cnt agts edges])
(extend StructuredPopulation
  Population
  (assoc timekeeping-behaviour
         ; draw a random edge and randomly flip speaker/listener roles
         :draw-pair (fn [this] (mapcat #(list % (nth (:agts this) %))
                                       ((if (zero? (rand-int 2))
                                          identity reverse)
                                         (rand-nth (:edges this)))))))

(defn connected?
  "Returns true iff the given set of edges defines a graph of n connected vertices"
  [n edges]
  (let [start (first (first edges))]
    (loop [visited #{}
           [head & tail] [start]]
      (if head
        (if (contains? visited head)
          (recur visited tail)
          (recur
            (conj visited head)
            (concat tail (map second (filter #(= head (first %)) edges)) (map first (filter #(= head (second %)) edges)))))
        (= (count visited) n)))))
;(connected? 3 [[0 1] [1 2]])
;(connected? 4 [[0 1] [1 2]])
;(connected? 4 [[0 1] [2 3]])
;(connected? 4 [[0 1] [1 2] [2 3]])

(defn new-randomly-connected-population
  "Creates a new randomly connected population of n agents, each
   of which is connected to exactly k other agents"
  [n k agent-vec-or-fn]
  {:pre [(>= k 2) (> n k)]}
  (let [agents (make-agent-vector n agent-vec-or-fn)
        init-pool (persistent! (reduce #(assoc! %1 %2 k) (transient {}) (range n)))]
    (loop [pool init-pool
           edges #{}]
      (case (count pool)
        ; we're done, just make sure the graph is connected, otherwise restart
        0 (if (connected? n edges)
            (StructuredPopulation. 0 agents (vec edges))
            (recur init-pool #{}))
        ; we're at a dead end, restart
        1 (recur init-pool #{})
        ; we're not done, randomly select two available sockets
        (let [[in out :as new] (sort (rand-nths (keys pool) 2))]
          ; check if this connection exists already
          (if (contains? edges new)
            ; if yes: we have a clash, make sure that the current network can still lead anywhere
            (if (or (= 1 (count pool))
                    (every? #(contains? edges %)
                            (for [x (keys pool) y (keys pool) :while (> x y)] (list y x))))
              ; we have reached a dead end, restart graph assembly from scratch
              (recur init-pool #{})
              ; just a clash, re-try random edge selection
              (recur pool edges))
            ; no clash, new edge can be added, go on
            (recur
              ; update socket pool (not the prettiest code)
              (let [newpool (if (= 1 (get pool in))
                              (dissoc pool in)
                              (update-in pool [in] dec))]
                (if (= 1 (get newpool out))
                  (dissoc newpool out)
                  (update-in newpool [out] dec)))
              ; add edge
              (conj edges new))))))))

; implementation of a production-perception loop that tracks the number of iterations
(defrecord Production-Perception-Loop [cnt agent])
(extend Production-Perception-Loop
  Population
  (assoc timekeeping-behaviour
         :size (fn [_] 1)
         :agents #(list (:agent %))
         :draw-pair #(vector 0 (:agent %) 0 (:agent %))
         :update-agents (fn [this outcome] (update-in this [:agent] #(update % :listener outcome)))))
