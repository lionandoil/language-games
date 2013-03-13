(ns ^{:author "kevin"}
  language-games.utils.math
  "Utility functions for mathematical operations"
  (:require
    [incanter.stats :only [sample-binomial]])
  (:use
    [incanter.optimize :only [non-linear-model]]))

; this can probably be made more efficient by basing the algorithm on random permutations
(defn rand-ints
  "Returns a seq of n DISTINCT random integers between 0 (inclusive) and
  m (exclusive) in random order."
  [m n]
  {:pre [(>= m n)]}
  (case n
    ; catch special cases
    1 [(rand-int m)]
    2 (let [f (rand-int m)]
        [f (mod (+ f 1 (rand-int (dec m))) m)])
    ; default clause for n > 2
    (if (= n m)
      (shuffle (range n))
      (take n (distinct (repeatedly #(rand-int m)))))))

(defn sample-binomial [n p]
  "Samples a single binomial, works also for p = 0.0 and 1.0"
  (case p
    0.0 0
    1.0 n
    ; work around incanter's buggy (sample-binomial) with (first)
    (first (incanter.stats/sample-binomial nil :size n :prob p))))

(defn limit
  "Limits x to lie within the range [mn,mx] (default [0,1]). Returns x
  if it lies within the range, mx if it is bigger, mn if it is smaller."
  ([x] (limit 0.0 1.0 x))
  ([mn mx x] (max mn (min mx x))))

(defn logistic
  "The logistic growth function with carrying capacity K, growth rate r and
  point of inflection t0"
  ([t] (logistic [1 1 0] t))
  ([[K r t0] t]
    (/ K (inc (Math/exp (- (* r (- t t0))))))))

(defn- guess-r-t0
  "Guess initial estimates of the logistic parameters r and t0 based on K and the data.

   Reference: http://home2.fvcc.edu/~dhicketh/DiffEqns/Activities/logistic.pdf"
  [K xs ys]
  ; find estimated turning point, i.e. first datapoint >= K/2
  (let [t0idx (first (keep-indexed #(when (>= %2 (/ K 2)) %1) ys))
        t0 (nth xs t0idx)
        ; find slope at the turning point to estimate growth rate r
        ; this will throw an exception if the data is malformed, e.g. if t0idx <= 0
        mt0 (/ (- (nth ys t0idx) (nth ys (dec t0idx)))
               (- t0             (nth xs (dec t0idx))))
        ; m'(t0) = K*r / 4
        ; r = 4*m'(t0) / K
        r (/ (* 4 mt0) K)]
    [r t0]))

(defn logistic-model
  "Attempts to fit a logistic function to the given datapoints.

   Reference: http://home2.fvcc.edu/~dhicketh/DiffEqns/Activities/logistic.pdf"
  ([K xs ys] ; with a fixed K
      (non-linear-model #(logistic (list* 1.0 %1) %2) ys xs (guess-r-t0 K xs ys)))
  ([xs ys]; & { :keys [K t0 r]}]
    (let [K (apply max ys)
          [r t0] (guess-r-t0 K xs ys)]
      (non-linear-model logistic ys xs [K r t0]))))
