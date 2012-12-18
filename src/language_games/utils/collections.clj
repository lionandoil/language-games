(ns ^{:author "kevin"}
     language-games.utils.collections
  "Utility functions for collections"
  (:use [language-games.utils.math :only [rand-ints]]
        [incanter.core :only [cumulative-sum]]))

(defn rand-nth-safe
  "Same as clojure.core.rand-nth, only that it returns nil when called on
an empty collection instead of throwing an IndexOutOfBoundsException."
  [coll]
  (if (next coll) (rand-nth coll) (first coll)))

(defn rand-nths
  "Returns n random elements from the collection which are taken from
n different indices in a random order. This is consequently NOT the
same as (repeatedly #(rand-nth coll) n)!"
  [coll n]
  (map (partial nth coll) (rand-ints (count coll) n)))

(defn rand-nths-indexed
  "Returns n random elements from the collection which are taken from n
different indices together with their indices in (i el_i j el_j ...) format"
  [coll n]
  (reduce #(conj %1 (nth coll %2) %2) '() (rand-ints (count coll) n)))
;  (mapcat #(do [% (nth coll %)]) (rand-ints (count coll) n)))



(defn rand-int-weighted
  "Returns a random integer i between 0 and (count weights) with probability
   proportional to (nth weights i). See also (incanter.stats.sample-multinomial)"
  [weights]
;  (when (seq weights)
  (let [cumsum (cumulative-sum weights)
        r (rand (peek cumsum))]
    (first (keep-indexed #(when (< r %2) %1) cumsum))))

(defn rand-nth-weighted
  "Returns a random element from coll with a probability proportional to the
   value found at the same index in weights."
  [coll weights]
  {:pre (= (count coll) (count weights))}
  (nth coll (rand-int-weighted weights)))



(defn indexed
  "Returns a sequence of (index item) pairs for the given collection, starting from 0"
  [coll]
  (map-indexed #(identity %&) coll))

(defn filter-indexed
  "Like filter, but returns a lazy seq of (idx item) pairs instead of just the items"
  [pred coll]
  (filter (fn [[_ elt]] (pred elt)) (indexed coll)))
;  (keep-indexed #(when (pred %2) [%1 %2]) coll))

(defn positions
  "Returns a lazy sequence of the indices of all elements which satisfy pred"
  [pred coll]
  (map first (filter-indexed pred coll)))
;  (for [[idx elt] (indexed coll) :when (pred elt)] idx))


(defn firsts
  "Returns a lazy seq of all consecutive elements at the beginning of the
collection for which the value of (key-fn %) is equal"
  ([coll] (firsts identity coll))
  ([key-fn [fi & res]]
    (cons fi (take-while #(= (key-fn fi) (key-fn %)) res))))
; a related function is "take-while-unstable":
; http://www.learningclojure.com/2011/01/take-while-unstable.html

(defn minimise
  "Returns one or more elements from the collection which minimise (key-fn). This
function differs from clojure.core/min-key by 1) possibly returning more than one
solution and 2) calculating key only once for each element."
  [key-fn [fi & res]]
  (loop [mins (list fi)
         value (key-fn fi)
         [fi & res] res]
    (if fi
      (case (compare value (key-fn fi))
        0 (recur (conj mins fi) value res)
        1 (recur (list fi) (key-fn fi) res)
        -1 (recur mins value res))
      mins)))

(defn maximise
  "Returns one or more elements from the collection which maximise (key-fn). This
function differs from clojure.core/max-key by 1) possibly returning more than one
solution and 2) calculating key only once for each element."
  [key-fn [fi & res]]
  (loop [maxs (list fi)
         value (key-fn fi)
         [fi & res] res]
    (if fi
      (case (compare value (key-fn fi))
        0 (recur (conj maxs fi) value res)
        -1 (recur (list fi) (key-fn fi) res)
        1 (recur maxs value res))
      maxs)))
