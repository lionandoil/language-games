(ns ^{:author "kevin"}
     language-games.utils.math
  "Utility functions for mathematical operations")

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
