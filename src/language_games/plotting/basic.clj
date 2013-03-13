(ns ^{:author "kevin"}
  language-games.plotting.basic
  "Useful functions for creating heat maps and basic plots of time series, typically in [0,1]"
  (:use [incanter.core :only [view]]
      	[incanter.charts :only [set-x-label set-y-label set-y-range xy-plot]]))

(defn basic-chart
  "Returns a new chart with a y-range of [0,1]"
  []
  (-> (xy-plot)
  	(set-y-range 0 1)))

(defn usm-chart
  "Returns a chart to plot usm data to"
  []
  (doto (basic-chart)
    (set-x-label "time")
    (set-y-label "innovation frequency")
    (view)))
