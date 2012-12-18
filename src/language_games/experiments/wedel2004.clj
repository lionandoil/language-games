(ns ^{:author "kevin"}
     language-games.experiments.wedel2004
  "Replication of Wedel 2004: Category competition drives contrast maintenance
   within an exemplar-based production/perception loop (Proceedings of the 7th
   Meeting of the ACL Special Interest Group in Computational Phonology)"
  (:use [language-games.core]
        [language-games.populations :only [production-perception-loop-behaviour]]
        [language-games.learning.exemplar :only [assemble-target
                                                 categorise-signal
                                                 replace-oldest-exemplar
                                                 replace-oldest-exemplar-corrective]]
        [incanter.core :only [abs dataset view]]
        [incanter.stats :only [mean median pdf-normal]];[euclidean-distance]]
        [incanter.charts :only [xy-plot trace-plot]]))

(def wedel2004-update-fn
  (partial language-game-update-skeleton
           #(vector (rand-int 2)) ; context-fn - only approximately the same as in the paper, see below
           first ; topic-fn
           nil ; no invention in this game
           ; utter-fn which adds +- 0.4 of noise but cuts off at 0.0 and 10.0
           #(median [0.0 (+ %2 (rand 0.8) -0.4) 10.0])))

(defn wedel2004-binary-categorisation-1d
  "Assigns a 1-dimensional signal to one of two categories: if the percept is 'behind' the
mean of one category it is consistently categorised as such, if it lies between the two
means it is assigned probabilistically proportional to the distance to the category means."
  [this _ percept]
  (let [cat-means (map mean (:exemplars this))
        [min max] (if (< (first cat-means) (second cat-means)) [0 1] [1 0])]
    (if (<= percept (nth cat-means min))
      min
      (if (>= percept (nth cat-means max))
        max
        (let [dists (abs (map #(- percept %) cat-means))]
          (if (< (first dists) (rand (apply + dists)))
            0 1))))))

; no inter-category competition (i.e. w/ corrective feedback)
(defrecord Wedel2004-corrective-feedback [exemplars])
(extend Wedel2004-corrective-feedback
  Agent
  {:produce assemble-target
   :interpret wedel2004-binary-categorisation-1d
   :update replace-oldest-exemplar-corrective}
  Population
  production-perception-loop-behaviour)

; inter-category competition (i.e. w/o corrective feedback)
(defrecord Wedel2004-category-competition [exemplars])
(extend Wedel2004-category-competition
  Agent
  {:produce assemble-target
   :interpret wedel2004-binary-categorisation-1d
   :update replace-oldest-exemplar}
  Population
  production-perception-loop-behaviour)

(defn run
  [rounds ppl]
  ; in the original all exemplars from both categories are completely replaced in one round. we can
  ; approximate this by making one round 20 interactions, so sometimes categories aren't sampled equally
  (let [gens (* 20 rounds)
        means (transient [[5.0 0] [5.0 1]])]
    ; only record category means every 20 interactions
    (add-watch *ppl* :cat-means (let [cnt (atom 0)]
                                  (fn [_ _ _ new]
                                    (swap! cnt inc)
                                    (when (zero? (mod @cnt 20))
                                      (conj!
                                        (conj! means [(mean (get-in new [:exemplars 0])) 0])
                                        [(mean (get-in new [:exemplars 1])) 1])))))
    (run-game-for wedel2004-update-fn gens ppl)
    (remove-watch *ppl* :cat-means)
    (view (xy-plot (interleave (range gens) (range gens)) :cat-mean
                   :data (dataset ["cat-mean" "cat"] (persistent! means))
                   :group-by :cat
                   :x-label "Rounds"))))

(defn -main
  "Creates the two graphs on page 3 of the paper"
  []
  ; 10 exemplars for each of the categories, all set to 5.0
  (let [initial-lexicon (apply vector (repeat 2 (apply vector (repeat 10 5.0))))]
    (time (run 2000 (->Wedel2004-corrective-feedback initial-lexicon)))
    (time (run 2000 (->Wedel2004-category-competition initial-lexicon)))))

(-main)
