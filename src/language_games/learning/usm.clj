(ns ^{:author "kevin"}
     language-games.learning.usm
  "The utterance-based selection model (Baxter et al. 2006, 2009) and
   its extension (Blythe & Croft 2012)"
  (:use [language-games.core :only [Agent update]]
        [clojure.math.numeric-tower :only [round]]
        [incanter.core :only [to-dataset col-names view]]
        [incanter.charts :only [xy-plot add-lines]]
        [incanter.stats :only [sample-binomial]]))

; parameters:
; * G(i,j), the interaction probabilities, are actually a property of the population
;           and thus not included in the agent learning/updating definitions
; * H(i,j)  the weighting function
; * T       the number of samples produced/observed by each agent in each interaction
; * f(n,T), the observed frequency -> perceived frequency function
; * lambda, the learning rate

(defn make-extended-usm-update-fn
  "Creates"
  [H T f lambda]
  (fn [this role {:keys [i j name interp]}]
    (case role
      :speaker
      (/
        (+ this
           (* lambda
              (+ (* (- 1 (H i j)) (f name T))
                 (* (H i j) (f interp T)))))
        (inc lambda))
      :listener
      (/
        (+ this
           (* lambda
              (+ (* (- 1 (H j i)) (f interp T))
                 (* (H j i) (f name T)))))
        (inc lambda)))))

;((make-extended-usm-update-fn (constantly 0.1) 4 / 0.01) 1.0 :speaker {:i 0 :j 0 :name 3 :interp 3})

(defn make-usm-update-fn
  "Where f(u) = u"
  [H T lambda]
  (make-extended-usm-update-fn H T / lambda))

(defn neutral-interactor-selection
  "Returns an USM updating function for neutral evolution with the given learning parameters"
  ([T lambda h]
    (neutral-interactor-selection T / lambda h))
  ([T f lambda h]
    (make-extended-usm-update-fn (constantly (* lambda h)) T f lambda)))

(defn weighted-interactor-selection
  "Returns an USM updating function for leader-follower models with n leaders"
  [T lambda h n alpha]
  (make-extended-usm-update-fn #(* lambda h (if (and (> %1 n) (<= %2 n))
                                              alpha 1.0))
                               T / lambda))

(defn replicator-selection
  "Returns an USM updating function with the innovation asymetrically preferred
   relative to parameter b"
  [T b lambda h]
  {:pre [(pos? b)]}
  (make-extended-usm-update-fn (constantly (* lambda h)) T #(min [1 (* (inc b) %)]) lambda))


(defn- sample-bin [n p]
  "Samples a single binomial, works also for p = 0.0 and 1.0"
  (case p
    0.0 0
    1.0 n
    ; work around incanter's buggy (sample-binomial) with (first)
    (first (sample-binomial nil :size n :prob p))))

(defn make-doubles-usm-agents
  "Calls (extend) on java.lang.Double so that doubles will be treated as agents
   with the given utterance-based selection model update function."
  [T update-fn]
  (extend java.lang.Double
    Agent
    {:produce (fn [this _ _] (sample-bin T this))
     :interpret (fn [this _ _] (sample-bin T this))
     :update update-fn}))

(defn make-bigdecimals-usm-agents
  "Calls (extend) on java.math.BigDecimal so that arbitrary precision decimals will
   be treated as agents with the given utterance-based selection model update function."
  [T update-fn]
  (extend java.math.BigDecimal
    Agent
    {:produce (fn [this _ _] (sample-bin T (double this)))
     :interpret (fn [this _ _] (sample-bin T (double this)))
     :update update-fn}))
