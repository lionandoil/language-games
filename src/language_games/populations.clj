(ns ^{:author "kevin"}
     language-games.populations
  "Some basic implementations of the language-games.core.Population protocol"
  (:use [language-games.core]
        [language-games.utils.collections :only [rand-nths-indexed]]))

(def production-perception-loop-behaviour
  "Default implementations for running games with a single agent in a production-perception loop,
   associate by invoking (extend <YourAgentType> Population production-perception-loop-behaviour)."
  {:size (fn [_] 1)
   :agents list
   :draw-pair #(vector 0 % 0 %)
   :update-agents (fn [this outcome] (update this :listener outcome))
   :advance-population identity})


(def seqable-behaviour
  "Default implementations for running language games on seqable collection of agents which
   are simply treated as a fully connected population. Does not implement timekeeping/iteration
   count tracking."
  {:iteration (fn [_] nil)
   :size count
   :agents identity
   :draw-pair (fn [this] (rand-nths-indexed this 2)) ; fully connected
   ; default behaviour: update speaker and listener
   :update-agents (fn [this {:keys [i j name] :as outcome}]
                    (if (nil? name)
                      (-> this ; new name invented, update both
                        (update-in [i] #(update % :speaker outcome))
                        (update-in [j] #(update % :listener outcome)))
                      (update-in this [j] #(update % :listener outcome))))
   :advance-population identity}); no timekeeping/population update


; we can use simple vectors to represent fully connected populations without turnover
(extend clojure.lang.PersistentVector
  Population
  seqable-behaviour)
(defn new-closed-population
  "Creates a new fully connected population with n agents initialised by calling new-agent-fn."
  [n new-agent-fn]
  (vec (repeatedly n new-agent-fn)))

; sensible default implementations for a few methods that works for defrecords
; defining the two fields cnt (an int counter) and agts (a seq of agents)
(def timekeeping-behaviour {:iteration (fn [this] (:cnt this))
                            :size (fn [this] (count (:agts this)))
                            :agents (fn [this] (:agts this))
                            :draw-pair (fn [this] (rand-nths-indexed (:agts this) 2))
                            :update-agents (fn [this [i j _ _ name & _ :as outcome]]
                                             (if (nil? name)
                                               (-> this ; new name invented, update both
                                                 (update-in [:agts i] #(update % :speaker outcome))
                                                 (update-in [:agts j] #(update % :listener outcome)))
                                               (update-in this [:agts j] #(update % :listener outcome))))
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
  (OpenPopulation. new-agent-fn turnover 0 0 (vec (repeatedly n new-agent-fn))))

; implementation of a production-perception loop that tracks the number of iterations
(defrecord Production-Perception-Loop [cnt agent])
(extend Production-Perception-Loop
  Population
  (assoc timekeeping-behaviour
         :size (fn [_] 1)
         :agents #(list (:agent %))
         :draw-pair #(vector 0 (:agent %) 0 (:agent %))
         :update-agents (fn [this outcome] (update-in this [:agent] #(update % :listener outcome)))))
