(ns ^{:author "kevin"}
     language-games.core
  "A Clojure framework for running all sorts of language game experiments"
  (:use [language-games.utils.collections :only [rand-nth-safe]]))

; a language game takes three ingredients:

; 1. an agent strategy specification which includes:
;  - a production strategy
;  - an interpretation strategy
;  - an updating (learning) strategy
(defprotocol Agent
  "Protocol for language game agents which bundles together a production, interpretation and learning strategy"
  (produce [this ctx topic] "Returns an [utterance candidate-list] tuple")
  (interpret [this ctx utterance] "Returns a seq of interpretations for the given utterance with descending probability")
  (update [this role outcome] "Returns an updated version of this agent given the interaction outcome"))

(defn rand-name [candidates]
  "A useful function that randomly selects an element from a candidates list and
   returns it in (Agent/produce) format. Used in a number of Agent-implementations."
  [(rand-nth-safe candidates) candidates])

; 2. a population in which these agents are organised (which could
;    also be a just single agent in a production-perception-loop!)
(defprotocol Population
  "Population protocol used to scaffold different population structures/organisations."
  (iteration [this] "Returns the current generation number of this population, or nil.")
  (size [this] "Returns the number of agents in the population")
  (agents [this] "Returns a seq of all agents in this population")
;  (random-agent [this] "Returns a random agent from the population")
  (draw-pair [this] "Returns a quadruple (i ag_i j ag_j) signifying two agents from
                     the population which are ready to engage in a local interaction.")
  (pairs [this] "Returns a seq of all quadruples (i ag_i j ag_j) that could potentially
                 be returned by (draw-pair). Used to measure overall communicative success
                 in structured populations.")

  ; functions which result in changes to the population or population structure and hence return the updated population
  (update-agents [this outcome] "Returns the population with agents updated according to the
interaction outcome. Additional outcome calculations can be performed for the agent updates.")
  ; functions which have changes to the population structure as a side-effect
  (advance-population [this n] "Advance the population by n timesteps (implementation-dependent)"))

; 3. an interaction script, including:
;  - a context generation function ( -> context)
;  - a topic selection function (context -> topic)
;  - in case production fails, a name invention function (speaker context topic -> new-name)
;  - an utterance function (name -> utterance) which might alter the signal selected by the speaker

; these refs can be used for monitoring by means of (add-watch) to register loggers etc.
(def ^:dynamic *outcome* (atom nil))
(def ^:dynamic *ppl* (atom nil))

(defrecord Outcome [i j ctx topic name utt interp])

(defn naming-game-update-skeleton
  "Skeleton for a naming game updating function.
   The most idiomatic way to use this is to first
   (def game (partial language-game-update-skeleton fn fn fn fn))
   and consequently run the game using (run-game-for game _ _)
   or a similar function."
  [ctxt-fn topic-fn invent-fn utter-fn ppl]
  (let
    [[i speaker j listener] (draw-pair ppl)
     ctx (ctxt-fn); speaker
     topic (topic-fn ctx); speaker
     [choice _] (produce speaker ctx topic)
     name (or choice (invent-fn speaker ctx topic))
     utt (utter-fn speaker name)
     interp (interpret listener ctx utt)
;     success (= topic interp) ; TODO apply general measures
     outcome (->Outcome i j ctx topic choice utt interp)]
    (swap! *outcome* (constantly outcome))
    (swap! *ppl* (fn [_] (update-agents ppl outcome)))))

(defn coordination-game-update-fn
  "Updating function for language games where no referential
   information is transferred and where there's consequently
   no communicative context - both 'speaker' and 'listener'
   get called with nil-arguments instead of contexts/topics.
   The function simply draws a pair of agents, calls produce and
   interpret (which could in fact be more of a second production
   function, as in the USM) on them and updates the agents and
   population accordingly. This function can be used for the
   minimal naming game or the utterance-based selection model."
  [ppl]
  (let
    [[i speaker j listener] (draw-pair ppl)
     choice (produce speaker nil nil)
     interp (interpret listener nil choice)
     outcome (->Outcome i j nil nil choice nil interp)]
    (swap! *outcome* (constantly outcome))
    (swap! *ppl* (fn [_] (update-agents ppl outcome)))))

(defn run-game-for
  "Runs game-fn gens times, starting with and continually updating ppl"
  ([game-fn gens] (run-game-for game-fn gens *ppl*))
  ([game-fn gens ppl]
    {:pre [(pos? gens)]}
    ; iterate is maybe not ideal since it requires game-fn to have no side
    ; effects, not sure how true this is with the two dynamic variables..?
    (nth (iterate game-fn ppl) gens)))

(defn run-until-convergence
  "Runs game-fn indefinitely (or for at most maxgens generations) until
   (converged? *ppl*) returns true"
  ([game-fn converged? init-ppl]
    (loop [ppl init-ppl]
      (if (converged? ppl)
        ppl
        (recur (game-fn ppl)))))
  ([game-fn converged? maxgens init-ppl]
    {:pre [(pos? maxgens)]}
    (loop [g 0
           ppl init-ppl]
      (if (or (converged? ppl) (= maxgens g))
        ppl
        (recur (inc g) (game-fn ppl))))))
