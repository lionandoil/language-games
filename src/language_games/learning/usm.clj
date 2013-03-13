(ns ^{:author "kevin"}
     language-games.learning.usm
  "The utterance-based selection model (Baxter et al. 2006, 2009) and
   its extension (Blythe & Croft 2012)"
  (:use [language-games.core :only [Agent agents update]]
        [language-games.utils.collections :only [take-while-strictly-monotonic]]
        [language-games.utils.math :only [sample-binomial]]))

; parameters:
; * G(i,j), the interaction probabilities, are actually a property of the population
;           and thus not included in the agent learning/updating definitions
; * H(i,j)  the weighting function
; * T       the number of samples produced/observed by each agent in each interaction
; * f(n,T), the observed frequency -> perceived frequency function
; * lambda, the learning rate

; the extended USM update function (Blythe & Croft 2012):
; x' = [ x + lambda * ( (1-Hij)*f(ni) + Hij*f(nj) ) ] / (1 + lambda)
(defn make-extended-usm-update-fn
  "Returns an extended utterance selection model update function with the given
   parameters, where 0 <= (H i j) <= 1"
  [H T f lambda]
  (fn [this role {:keys [i j name interp]}]
    (let [[self other h] (case role :speaker [name interp (H i j)] :listener [interp name (H j i)])]
      (/
        (+ this
           (* lambda
              (+ (* (- 1 (H i j)) (f self T))
                 (* (H i j) (f other T)))))
        (inc lambda)))))

; the original USM (Baxter et al. 2006, 2009) update function:
; x' = [ x + lambda * (ni + Hij*nj) ] / (1 + lambda * (1+Hij) )
(defn make-usm-update-fn
  "Returns an original utterance selection model update function with the given
   parameters, where 0 <= (H i j) < infinity"
  [H T lambda]
  (fn [this role {:keys [i j name interp]}]
    (let [[self other h] (case role :speaker [name interp (H i j)] :listener [interp name (H j i)])]
      (/
        (+ this
           (* lambda
              (+ (/ self T)
                 (* h (/ other T)))))
        (inc (* (inc h) lambda))))))

(defn neutral-interactor-selection
  "Returns an original (when no f is given) or extended USM update function
  (when f is specified) for neutral evolution"
  ([T lambda h]
    (make-usm-update-fn (constantly h) T lambda))
  ([T f lambda h]
    (make-extended-usm-update-fn (constantly h) T f lambda)))

(defn weighted-interactor-selection
  "Returns an extended USM update function for leader-follower models with n leaders"
  [T lambda h n alpha]
  (make-extended-usm-update-fn
    #(if (and (> %1 n) (<= %2 n))
         (* h alpha)
         h)
    T lambda))

(defn replicator-selection
  "Returns an extended USM update function with the innovation asymetrically preferred
   relative to parameter b"
  [T lambda h b]
  {:pre [(pos? b)]}
  (make-extended-usm-update-fn (constantly h) T #(min 1 (* (inc b) (/ %1 %2))) lambda))

(defn make-doubles-usm-agents
  "Calls (extend) on java.lang.Double so that doubles will be treated as agents
   with the given utterance-based selection model update function."
  [T update-fn]
  (extend java.lang.Double
    Agent
    {:produce (fn [this _ _] (sample-binomial T this))
     :interpret (fn [this _ _] (sample-binomial T this))
     :update update-fn}))

(defn strict-convergence
  "Returns true iff all agents in the population have the same value of x"
  [ppl]
  (apply = (agents ppl)))

(defn approximate-convergence
  "Returns true iff all agents in the population are within 10E-6 of categorical use of one variant"
  [ppl]
  (every?
    (if (> (first (agents ppl)) 0.5) #(> % (- 1 10E-6))
                                     #(< % 10E-6))
    (agents ppl)))
