(ns ^{:author "kevin"}
  language-games.logging
  "Useful macros for collecting, storing and plotting simulation data (on the fly)"
  (:use [incanter.core :only [dataset conj-rows set-data to-dataset]]
        [incanter.charts :only [add-lines set-legend]]))

(defmacro with-collect
  "Collects any changes to var in one or more seq atoms.

  Usage: (with-collect [*ppl* init-ppl {means #(mean (agents %)) maxs #(max (agents %))} :every 20]
            (do stuff that causes changes to *ppl*
              use the atoms means, maxs etc to access accumulated data))"
  [[var var-fn-map & {:keys [every init] :or {every 1}}] & body]
  ; make a variable for every keyword/fn
  `(let ~(vec (mapcat #(do `(~(first %) ~(if (nil? init)
                                           `(atom [])
                                           `(atom [(~(second %) ~init)])))) var-fn-map))
    (add-watch ~var :collect
      ; if every is a constant 1 we can eliminate the need for a counter
      ~(if (= every 1) 
       `(fn [_# _# _# new]
          ~@(map (fn [[k v]] `(swap! ~k conj (~v new))) var-fn-map))
       `(let [cnt# (atom 1)]
          (fn [_# _# _# new]
            (swap! cnt# inc)
            (when (zero? (mod @cnt# ~every))
              (swap! cnt# (constantly 0))
              ~@(map (fn [[k v]] `(swap! ~k conj (~v new))) var-fn-map))))))
    (let [ret# (do ~@body)]
      (remove-watch ~var :collect)
      ret#)))
;(macroexpand '(with-collect [*foo* {foo mean bar max} :every 2 :init 123] 1 2 3))

(defmacro with-collect-data
  "Incrementally collects data from changes to a var into an incanter dataset.

  x is an optional lazy sequence of x-indices that has to be available at
  compile time which will be put into column :x (defaults to (range)).

  Usage: (with-collect-data [*ppl* my-dataset {:mean #(mean (agents %)) :max #(max (agents %))} :every 20]
          (do stuff that causes changes to *ppl* and look at data in @my-dataset))"
  [[var dataset-var key-fn-map & {:keys [x every init] :or {every 1}}] & body]
  `(let [~dataset-var (atom (dataset (conj (keys ~key-fn-map) :x) (when ~init [(conj (map #(% ~init) (vals ~key-fn-map)) (first (or ~x (range))))])))]
    (add-watch ~var :collect-data
      ; if every is a constant 1 we can eliminate the need for a counter
      ~(if (= every 1) 
       `(fn [_# _# _# new]
          (swap! ~dataset-var #(conj-rows % [~(mapv (fn [[k v]] `(~v new)) key-fn-map)])))
       `(let [cnt# (atom 1)
              xs# (atom (rest (or ~x (range))))]
          (fn [_# _# _# new]
            (swap! cnt# inc)
            (when (zero? (mod @cnt# ~every))
              (swap! cnt# (constantly 0))
              (swap! ~dataset-var #(conj-rows % [(first @xs#) ~@(map (fn [[k v]] `(~v new)) key-fn-map)]))
              (swap! xs# rest))))))
    (let [ret# (do ~@body)]
      (remove-watch ~var :collect-data)
      ret#)))
;(macroexpand '(with-collect-data [*ppl* dataset {:foo mean :bar max} :init [1 2 3]] (println "foo")))

(defmacro with-plot-data
  "Adds a watcher to the dataset var that applies (set-data) to
  the given chart every time the dataset is updated.

  Usage: (with-plot-data [chart dataset [:some-column :other-column] :x :x-column]
           (do stuff that alters the dataset-var, causing the chart to be updated accordingly))"
  [[chart dataset-var columns & {:keys [x] :or {x :x}}] & body]
  `(let [watcher-id# (gensym "plot-data")
         chart# ~chart]
    (doseq [col# ~columns]
      (add-lines chart# ~x col# :data @~dataset-var :series-label (str col#)))
    (set-legend chart# true)
    (add-watch ~dataset-var watcher-id# (fn [_# _# _# new]
                                          (dorun (map-indexed (fn [i# col#] (set-data chart# ~x col# new i#)) ~columns))))
     (let [ret# (do ~@body)]
       (remove-watch ~dataset-var watcher-id#)
       ret#)))
