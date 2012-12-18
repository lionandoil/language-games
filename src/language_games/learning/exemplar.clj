(ns ^{:author "kevin"}
     language-games.learning.exemplar
  "Customisable functions for exemplar-based agents to use in language games with continuous form spaces"
  (:use [language-games.core :only [Agent rand-name]]
        [language-games.utils.collections :only [maximise rand-nths rand-int-weighted]]
        [incanter.stats :only [mean]]))

; target assembly

(defn assemble-target
  "Assembles a target and returns it in (Agent/produce) format by
   randomly selecting an exemplar from the target category."
  [this _ category]
  (rand-name (get-in this [:exemplars category])))

(defn assemble-target-blend-random
  "Assembles a target and returns it in (Agent/produce) format by
   blending n random exemplars from the target category."
  [n this _ category]
  [(mean (rand-nths (get-in this [:exemplars category]) n))
   (get-in this [:exemplars category])])

;(defn assemble-target-blend-nearby
;  [n this _ category]
;  (let [target (rand-nth category-exemplars)]
;    [target category-exemplars])); TODO select neighbours


; categorisation

(defn categorise-signal
  "Skeleton for an exemplar-categorisation function.
   (match-fn percept exemplar) returns some distance between the percept and exemplar
   (average-fn match-distances) determines the overall match of a percept with the category, based on
                                the distances between the percept and all exemplars of the category
   (choice-fn cat-averages) determines the winner based on the overall matches of all categories and
                            returns the index of the category, or nil in case of categorisation failure"
  [choice-fn average-fn match-fn this _ signal]
  (let [cat-weights (map average-fn (map (partial match-fn signal) (:exemplars this)))]
    [(choice-fn cat-weights) nil])); TODO also return competitors, ordered by their weights

(def categorise-by-matches
  (partial categorise-signal
           rand-int-weighted ; stochastic category selection proportional to the
                             ; number of matches, with no categorisation failure
           +)) ; category average: count number of matches

(def categorise-by-matches-maximise
  (partial categorise-signal
           (partial maximise #(do nil)) ; TODO deterministic category selection, no categorisation failure
           +)) ; category average: count number of matches

; storage & replacement

(def replace-oldest-exemplar
  (fn [this role {:keys [utt interp]}]
    (if (= role :speaker)
      this
      (update-in this [:exemplars interp] #(conj (subvec % 1) utt)))))

(def replace-oldest-exemplar-corrective
  (fn [this role {:keys [utt topic]}]
    (if (= role :speaker)
      this
      (update-in this [:exemplars topic] #(conj (subvec % 1) utt)))))

(def replace-random-exemplar
  (fn [this role {:keys [utt interp]}]
    (if (= role :speaker)
      this
      (update-in this [:exemplars interp] #(assoc % (rand-int (count %)) utt)))))

(def replace-random-exemplar-corrective ; corrective feedback, or 'deixis' as in Wedel 2004
  (fn [this role {:keys [utt topic]}]
    (if (= role :speaker)
      this
      (update-in this [:exemplars topic] #(assoc % (rand-int (count %)) utt)))))
