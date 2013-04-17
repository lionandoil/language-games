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

(defn heat-map
  "Modification of incanter.heat-map that takes actual data instead of a function.
  The data has to be a sequence of 3 sequences of equal length, which are then
  treated as the x, y, and z values respectively (such as supplied by grid-apply)"
  ([data & options]
     (let [opts (apply hash-map options)
           color? (if (false? (:color? opts)) false true)
           title (or (:title opts) "")
           x-label (or (:x-label opts) "")
           y-label (or (:y-label opts) "")
           z-label (or (:z-label opts) "z scale")
           theme (or (:theme opts) :default)
           xyz-dataset (org.jfree.data.xy.DefaultXYZDataset.)
           data (into-array (map double-array data))
           min-z (reduce min (last data))
           max-z (reduce max (last data))
           x-axis (doto (org.jfree.chart.axis.NumberAxis. x-label)
                    (.setStandardTickUnits (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits))
                    (.setLowerMargin 0.0)
                    (.setUpperMargin 0.0)
                    (.setAxisLinePaint java.awt.Color/white)
                    (.setTickMarkPaint java.awt.Color/white))
           y-axis (doto (org.jfree.chart.axis.NumberAxis. y-label)
                    (.setStandardTickUnits (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits))
                    (.setLowerMargin 0.0)
                    (.setUpperMargin 0.0)
                    (.setAxisLinePaint java.awt.Color/white)
                    (.setTickMarkPaint java.awt.Color/white))
           colors (or (:colors opts)
                      [[0 0 127] [0 0 212] [0 42 255] [0 127 255] [0 127 255]
                       [0 226 255] [42 255 212] [56 255 198] [255 212 0] [255 198 0]
                       [255 169 0] [255 112 0] [255 56 0] [255 14 0] [255 42 0]
                       [226 0 0]])
           scale (if color?
                   (org.jfree.chart.renderer.LookupPaintScale. min-z max-z java.awt.Color/white)
                   (org.jfree.chart.renderer.GrayPaintScale. min-z max-z))
           add-color (fn [idx color]
                       (.add scale
                             (+ min-z (* (/ idx (count colors)) (- max-z min-z)))
                             (apply #(java.awt.Color. %1 %2 %3) color)))
           scale-axis (org.jfree.chart.axis.NumberAxis. z-label)
           legend (org.jfree.chart.title.PaintScaleLegend. scale scale-axis)
           renderer (org.jfree.chart.renderer.xy.XYBlockRenderer.)

           plot (org.jfree.chart.plot.XYPlot. xyz-dataset x-axis y-axis renderer)
           chart (org.jfree.chart.JFreeChart. plot)]
       (do
        (.setPaintScale renderer scale)
        (when color? (doseq [i (range (count colors))]
                       (add-color i (nth colors i))))
        (.addSeries xyz-dataset "Series 1" data)
        (.setBackgroundPaint plot java.awt.Color/lightGray)
        (.setDomainGridlinesVisible plot false)
        (.setRangeGridlinePaint plot java.awt.Color/white)
        (.setAxisOffset plot (org.jfree.ui.RectangleInsets. 5 5 5 5))
        (.setOutlinePaint plot java.awt.Color/blue)
        (.removeLegend chart)
        (.setSubdivisionCount legend 20)
        (.setAxisLocation legend org.jfree.chart.axis.AxisLocation/BOTTOM_OR_LEFT)
        (.setAxisOffset legend 5.0)
        (.setMargin legend (org.jfree.ui.RectangleInsets. 5 5 5 5))
        (.setFrame legend (org.jfree.chart.block.BlockBorder. java.awt.Color/red))
        (.setPadding legend (org.jfree.ui.RectangleInsets. 10 10 10 10))
        (.setStripWidth legend 10)
        (.setPosition legend org.jfree.ui.RectangleEdge/RIGHT)
        (.setTitle chart title)
;        (.addSubtitle chart legend)
        (org.jfree.chart.ChartUtilities/applyCurrentTheme chart))
;        (set-theme chart theme))
       chart)))
