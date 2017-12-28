;; gorilla-repl.fileformat = 1

;; **
;;; # Distributions Worksheet
;; **

;; @@
(ns distributions-worksheet
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(->> (for [_ (range 10000)]
       (repeatedly 100 #(if (<= (rand) 0.99) 1 0)))
     (map (partial apply +))
     (frequencies)
     (sort)
     (plot/list-plot)
     )
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2188,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"05cabaf1-2b1d-45d9-ae43-c57e64d91716","values":[{"x":94,"y":3},{"x":95,"y":35},{"x":96,"y":157},{"x":97,"y":669},{"x":98,"y":1845},{"x":99,"y":3686},{"x":100,"y":3605}]}],"marks":[{"type":"symbol","from":{"data":"05cabaf1-2b1d-45d9-ae43-c57e64d91716"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"05cabaf1-2b1d-45d9-ae43-c57e64d91716","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"05cabaf1-2b1d-45d9-ae43-c57e64d91716","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"05cabaf1-2b1d-45d9-ae43-c57e64d91716\", :values ({:x 94, :y 3} {:x 95, :y 35} {:x 96, :y 157} {:x 97, :y 669} {:x 98, :y 1845} {:x 99, :y 3686} {:x 100, :y 3605})}], :marks [{:type \"symbol\", :from {:data \"05cabaf1-2b1d-45d9-ae43-c57e64d91716\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"05cabaf1-2b1d-45d9-ae43-c57e64d91716\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"05cabaf1-2b1d-45d9-ae43-c57e64d91716\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
