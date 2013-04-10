(ns ^{:author "kevin"}
  language-games.experiments.blythecroft2012
  "Replications of the figures from Blythe & Croft (2012):
  S-curves and the mechanisms of propagation in language change"
  (:use [language-games.core]
        [language-games.populations :only [make-agent-vector
                                           new-randomly-connected-population]]
        [language-games.learning.usm]
        [language-games.logging :only [with-collect-data with-plot-data]]
        [language-games.plotting.basic :only [usm-chart]]
        [language-games.utils.math :only [sample-binomial]]
        [clojure.math.numeric-tower :only [round]]
        [incanter.pdf :only [save-pdf]]
        [incanter.stats :only [mean]]
        [incanter.core :only [abs sq dataset view save set-data]]
        [incanter.charts :only [xy-plot set-legend set-x-label set-y-label set-y-range add-function add-lines add-pointer]]))

(defn run
  "If rounds is nil, runs until the entire population converges on the same value of x."
  [lambda rounds ppl & {:keys [chart convergence-test resolution] ; resolution = datapoints per round
                        :or {convergence-test approximate-convergence resolution 1}}]
  (let [roundsize (int (/ (size ppl) (* lambda lambda)))
        chart (or chart (usm-chart))]
    (println "roundsize" roundsize)
    (with-collect-data [*ppl* dataset {:means #(mean (agents %))} :x (range 0 (Integer/MAX_VALUE) (/ 1.0 resolution)) :every (/ roundsize resolution) :init ppl]
      (with-plot-data [chart dataset [:means]]; :maxs :mins]]
        (if convergence-test
          (run-until-convergence coordination-game-update-fn convergence-test (* roundsize rounds) ppl)
          (run-game-for coordination-game-update-fn (* roundsize rounds) ppl))
        chart))))

; neutral evolution/neutral interactor selection
(defn figure2 []
  (let [N 80
        k 8
        p 0.2
        lambda 0.01
        h 1.0
        T 4
        chart (usm-chart)]
    (make-doubles-usm-agents T (neutral-interactor-selection T / lambda h))
;    (dotimes [i 4]
    (run lambda 100
      (new-randomly-connected-population N k #(double (sample-binomial 1 p)))
      :resolution 10
      :chart chart)));)
;(figure2)
;(save-pdf *1 "figure2.pdf")

; neutral interactor selection with nonlinear sampling rules
(defn figure3 []
  (let [N 80
        k 8
        lambda 0.01
        h 1.0
        T 4]
      (println "a=0.02")
      (make-doubles-usm-agents T
        (neutral-interactor-selection T #(let [u (/ %1 %2)] (+ u (* 0.02 u (- 1 u) (dec (* 2 u))))) lambda h))
      (run lambda 20 (new-randomly-connected-population N k #(double (sample-binomial 1 0.5)))
        :resolution 10)
      (println "a=-0.02")
      (make-doubles-usm-agents T
        (neutral-interactor-selection T #(let [u (/ %1 %2)] (+ u (* -0.02 u (- 1 u) (dec (* 2 u))))) lambda h))
      (run lambda 30 (new-randomly-connected-population N k #(double (sample-binomial 1 0.2)))
        :resolution 10)))
;(figure3)

; weighted interactor selection
(defn figure4 []
  (let [N 80
        k 8
        n 5
        lambda 0.01
        h 1.0
        T 4]
    (doseq [alpha [20 50]]
      ; TODO find formula for characteristic frequency (cf. caption of Figure 4, p.288)
      (let [;characteristic-frequency ()
            chart (usm-chart)]
;        (add-lines chart [0 20] [characteristic-frequency characteristic-frequency])
        (println "alpha" alpha)
        (make-doubles-usm-agents T (weighted-interactor-selection T lambda h n alpha))
        ; leaders start with strict use of the innovation
        (run lambda 20 (new-randomly-connected-population N k (vec (concat (repeat n 1.0)
                                                                           (repeat (- N n) 0.0))))
          :resolution 10
          :chart chart)))))
;(figure4)

; replicator selection!
(defn figure7 []
  (let [N 80
        k 8
        p 0.1
        b 0.001
        lambda 0.01
        h 1.0
        T 4]
    (make-doubles-usm-agents T (replicator-selection T lambda h b))
    (-> (run lambda 40 (new-randomly-connected-population N k #(double (sample-binomial 1 p)))
      :resolution 10)
      ; this is not fitted individually and will thus deviate from an optimal fit
      ; due to the stochastic nature of the initial transition pickup!
      (add-function (partial replicator-selection-analytic [T p lambda b]) 0.0 25.0))))
;(figure7)
