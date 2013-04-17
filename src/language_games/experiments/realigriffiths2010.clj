(ns ^{:author "kevin"}
  language-games.experiments.realigriffiths2010
  "Bayesian Iteratead Learning and Fisher-Wright"
  (:use [language-games.plotting.basic :only [heat-map]]
        [language-games.utils.stats :only [pdf-beta-binomial sample-binomial]]
        [incanter.core :only [$= matrix mmult trans view save]]
        [incanter.charts :only [add-lines xy-plot]]
        [incanter.stats :only [mean pdf-binomial]]))

(defn bilm-transition-matrix
  "Create the NxN Markov chain transition matrix for the given population size and alpha"
  [N alpha]
  (let [freqs (range (inc N))]
    (matrix (map #(pdf-binomial freqs :size N :prob ($= (% + alpha / 2) / (N + alpha))) freqs))))

(defn wright-fisher-transition-matrix
  "Returns the transition matrix for the wright-fisher model with the given
  population size N and mutation rate u, based on the step-wise population
  update which is sampling from a binomial n = N, p = ((1-u)x + u(N-x))/N"
  [N u]
  (let [freqs (range (inc N))]
    (matrix (map #(pdf-binomial freqs :size N :prob ($= ((1 - u) * % + u * (N - %)) / N)) freqs))))

; sanity checks
(map #(apply + %) (bilm-transition-matrix 10 2))
(map #(apply + %) (wright-fisher-transition-matrix 10 (/ 1 12)))

(defn N-alpha-to-u
  "Calculates the mutation rate for the Wright-Fisher model that makes
  it equivalent to Bayesian iterated learning with the given N and alpha.
  The bracketing in the paper itself is garbled and there's a / missing
  somewhere, the correct version of the transformation can be found on
  page 5 (top) of the supplementary material."
  [N alpha]
  ($= alpha / (2 * (alpha + N))))
;(N-alpha-to-u 10 0.2)
;(N-alpha-to-u 10 2.0)
;(N-alpha-to-u 10 10.0)

; the two transition matrices are in fact identical 
(= (bilm-transition-matrix 10 2) (wright-fisher-transition-matrix 10 (N-alpha-to-u 10 2)))


; prepare a few plotting functions to reproduce the graphs from the paper

; the z-scale of the heatmap plots isn't specified in the paper, so we'll
; have to cook up our own log-ish scale
(defn to-heatmap-data
  "Transform a seq of seqs representing the development of distribution
  over time into a seq of 3 seqs with x & y coordinates and z values"
  [N data]
  [(mapcat #(repeat (inc N) %) (range (count data)))
   (take (* (count data) (inc N)) (cycle (range (inc N))))
   (map #(- (Math/log10 (+ 0.0002 %))) (flatten data))])
(defn bilm-heatmap
  [data]
  (let [hm (heat-map (to-heatmap-data (dec (count (first data))) data) :x-label "generation" :y-label "frequency" :color? false)]
  	(view hm)
  	hm))

(defn bilm-iteration
  "Carry out an iteration according to the stochastic process defined by iterated learning.
  dist is the current probability of occupied states, N is taken to be (count dist)-1 "
  [alpha dist]
  ; transpose so that every row contains all the probabilities of going into a state
  (mmult (trans (bilm-transition-matrix (dec (count dist)) alpha)) dist))
;  (mmult (trans (wright-fisher-transition-matrix (dec (count dist)) (N-alpha-to-u (dec (count dist)) alpha))) dist))

(bilm-heatmap (take 10 (iterate (partial bilm-iteration 0.2) [0 0 0 0 0 1.0 0 0 0 0 0])))
(bilm-heatmap (take 10 (iterate (partial bilm-iteration 2.0) [0 0 0 0 0 1.0 0 0 0 0 0])))
(bilm-heatmap (take 10 (iterate (partial bilm-iteration 10.0) [0 0 0 0 0 1.0 0 0 0 0 0])))


; 

(defn beta-stationary
  "Calculate the discretised version of the stationary distribution by
  looking at the beta-binomial distribution determined by the given beta
  distribution parameter"
  [para N]
  (map #(pdf-beta-binomial N para para %) (range (inc N))))

(defn wright-fisher-stationary
  "Returns the stationary distribution of the Wright-Fisher model with
  N genes and mutation rate u. As u -> 0 the Beta distribution becomes
  more strongly bimodal (all chains stay in one of the absorbing states)."
  [u N]
  (beta-stationary (* 2 N u) N))

(defn bilm-stationary
  "Returns the discretised version of the stationary distribution of the
  Bayesian iterated learning model with the given alpha and population
  size N. Bracketing in the paper is again implicit, the unambiguous
  version is on page 5 (bottom) of the supplementary material."
  [alpha N]
  (beta-stationary ($= alpha / (1 + alpha / N)) N))

;(view (xy-plot (range) (bilm-stationary 0.02 10)))
;(view (xy-plot (range) (bilm-stationary 2 10)))
;(view (xy-plot (range) (bilm-stationary 10 10)))

(defn append-stationary
  "Append a dummy column and a column with the stationary distribution
  to the data so they can be nicely compared in a heatmap"
  [data alpha]
  (let [N (count (first data))]
  	(concat data [(repeat N 0.0) (bilm-stationary alpha (dec N))])))

; have another look at the three markov chain developments in contrast to the stationary
;(bilm-heatmap (append-stationary (take 10 (iterate (partial bilm-iteration 0.2) [0 0 0 0 0 1.0 0 0 0 0 0])) 0.2))
;(bilm-heatmap (append-stationary (take 10 (iterate (partial bilm-iteration 2.0) [0 0 0 0 0 1.0 0 0 0 0 0])) 2.0))
;(bilm-heatmap (append-stationary (take 10 (iterate (partial bilm-iteration 10.0) [0 0 0 0 0 1.0 0 0 0 0 0])) 10.0))

; the posterior is completely uniform when alpha/(1+alpha/n) = 1,
(bilm-stationary (/ 10 9) 10)
(bilm-heatmap (append-stationary (take 10 (iterate (partial bilm-iteration (/ 10 9)) [0 0 0 0 0 1.0 0 0 0 0 0])) (/ 10 9)))



; section 4a: S-shaped curves in language change

(def homogeneous-population (list* 1.0 (repeat 50 0)))

; figure 2ai
(bilm-heatmap (append-stationary (take 50 (iterate (partial bilm-iteration 0.05) homogeneous-population)) 0.05))
;(view (xy-plot (range) (bilm-stationary 0.05 50)))

; figure 2bi
(bilm-heatmap (append-stationary (take 50 (iterate (partial bilm-iteration 10) homogeneous-population)) 10))
;(view (xy-plot (range) (bilm-stationary 10 50)))


; the probability that a chain that was initialised at 0/50 is all the way at 50/50 in the 50th generation is ~0.0006
(last (nth (iterate (partial bilm-iteration 0.05) homogeneous-population) 49))
; i.e. we'll get one of those every
(/ 1.0 (last (nth (iterate (partial bilm-iteration 0.05) homogeneous-population) 49)))
; time we stochastically run a chain


; let's numerically find some chains that fulfill this, i.e. let's do some actual iterated learning!

(defn population-update
  "Stochastic population update"
  [n alpha x]
  (sample-binomial n ($= (x + alpha / 2) / (n + alpha))))

(defn generate-fixation-chains
  "Generate chains until finding a fixed number that go from the original state (0/n) to fixation (n/n) in exact chainlength generations"
  [numchains chainlength n alpha]
  (let [p (last (nth (iterate (partial bilm-iteration alpha) (list* 1.0 (repeat n 0))) (dec chainlength)))]
  	(println "Probability that a chain initialised at 0 has gone to completion after" chainlength "generations is" p)
  	(println "This means one chain will be found every" (/ 1 p) "attempts, expect" (/ numchains p) "attempts until this loop exits"))
  (loop [i 0
  	     chains []]
    (if (= numchains (count chains))
      (do
        (println "Generating" numchains "transitions took" i "attempts," (double (/ i numchains)) "on average")
        chains)
      ; else generate more
      (let [chain (take chainlength (iterate (partial population-update n alpha) 0))]
        (if (= (last chain) n)
          (do (println "Found chain" (inc (count chains)) "in trial" (inc i))
          	(recur (inc i) (conj chains chain)))
          (recur (inc i) chains))))))

(defn plot-trajectory
  [chain]
  (let [chart (xy-plot (range) chain :x-label "generation" :y-label "frequency")]
    (view chart)
    chart))

; generate one and plot it
(plot-trajectory (first (generate-fixation-chains 1 50 50 0.05)))
;(save *1 "chain50.png")

; generate a bunch and look at the average trajectory (this will take a while)
(let [chains (generate-fixation-chains 25 50 50 0.05)
	   average (apply map #(mean %&) chains)]
(plot-trajectory average))

; what if we condition on completion at a later point?
(plot-trajectory (first (generate-fixation-chains 1 100 50 0.05)))
;(save *1 "chain100.png")
(plot-trajectory (first (generate-fixation-chains 1 150 50 0.05)))
;(save *1 "chain150.png")
(plot-trajectory (first (generate-fixation-chains 1 200 50 0.05)))
;(save *1 "chain200.png")
