(ns ^{:author "kevin"}
  language-games.utils.stats
  "Statistical functions not provided by incanter (e.g. the Beta-Binomial)"
  (:require
    [incanter.stats :only [sample-binomial]])
  (:use
    [incanter.core :only [$= beta choose]]))

(defn sample-binomial [n p]
  "Samples a single binomial, works also for p = 0.0 and 1.0"
  (case p
    0.0 0
    1.0 n
    ; work around incanter's buggy (sample-binomial) with (first)
    ((if (= n 1) identity first)
      (incanter.stats/sample-binomial nil :size n :prob p))))

(defn pdf-beta-binomial
  "Returns the probability mass of the Beta-Binomial distribution
  BB(n, alpha, beta) evaluated at x"
  [n alph bet x]
  {:pre [(integer? n) (integer? x) (>= x 0) (<= x n)]}
  (* (choose n x) (/ (beta (+ x alph) ($= n - x + bet)) (beta alph bet))))
