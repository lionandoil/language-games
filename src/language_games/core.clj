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
  (draw-pair [this] "Returns a quadruple (i ag_i j ag_j) signifying two agents
from the population which are ready to engage in a local interaction.")
  (pairs [this] "Returns a seq of all quadruples (i ag_i j ag_j) that could potentially
be returned by (draw-pair). Used to measure communicative success in structured populations.")

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
;(add-watch *outcome* :logging (fn [_ ref old new] (print new)))
;(add-watch *ppl* :convergence (partial convergence-watcher (range 3)))

(defrecord Outcome [i j ctx topic name utt interp success])

(defn language-game-update-skeleton
  "Skeleton for a language game updating function.
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
     success (= topic interp)
     outcome (->Outcome i j ctx topic choice utt interp success)]
    (swap! *outcome* (fn [_] outcome))
    (swap! *ppl* (fn [_] (update-agents ppl outcome)))))

(defn run-game-for
  [game-fn gens init-ppl]
  (loop [i 0
         ppl init-ppl]
    (if (= i gens)
      ppl
      (recur (inc i) (game-fn ppl)))))
