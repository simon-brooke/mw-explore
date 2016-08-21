;; gorilla-repl.fileformat = 1

;; **
;;; # Microworld Drainage Exploration
;;;
;;; This is a buffer I've set up to explore ideas about drainage.
;; **

;; @@
(ns mw-explore.mw-explore
  (:require [clojure.java.io :as io]
            [hiccup.core :refer [html]]
            [mw-engine.core :as engine]
            [mw-engine.heightmap :as heightmap]
            [mw-engine.drainage :as drainage]
            [mw-parser.bulk :as parser]
            ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Create a world - from a heightmap
;;;
;;; I'm using the 'medium' (8 Km per pixel) heightmap, to get some detail. This is slow but not disastrously so. This map is 256 pixels square.
;; **

;; @@
;;(time (def w0 (heightmap/apply-heightmap "../mw-ui/resources/public/img/heightmaps/great_britain_and_ireland_med.png")))
(time (def w0 (heightmap/apply-heightmap "../mw-ui/resources/public/img/heightmaps/great_britain_and_ireland_original.png")))
;;(time (def w0 (heightmap/apply-heightmap "../mw-ui/resources/public/img/heightmaps/great_britain_and_ireland_small.png")))
;;(time (def w0 (heightmap/apply-heightmap "../mw-ui/resources/public/img/heightmaps/small_hill.png")))
;; @@
;; ->
;;; &quot;Elapsed time: 1555.555578 msecs&quot;
;;;
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/w0</span>","value":"#'mw-explore.mw-explore/w0"}
;; <=

;; **
;;; ## Fill in any local hollows, and add rain
;;;
;;; At this stage, only detects/corrects single-cell hollows. Note that this takes a long time - on my (very fast) computer it takes quarter of an hour.
;; **

;; @@
(time (def w1 (drainage/rain-world (drainage/flood-hollows w0))))
;; @@
;; ->
;;; &quot;Elapsed time: 865276.685472 msecs&quot;
;;;
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/w1</span>","value":"#'mw-explore.mw-explore/w1"}
;; <=

;; **
;;; ## Next, trace the water flow
;;;
;;; The present algorithm adds the sum of the flow through all the cells for which the cell currently under consideration is the lowest neighbour, recursively uphill.  This is obviously inefficient and at present takes even longer.
;;;
;; **

;; @@
(time (def w2 (drainage/flow-world-nr w1)))
;; @@
;; ->
;;; &quot;Elapsed time: 950758.294506 msecs&quot;
;;;
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/w2</span>","value":"#'mw-explore.mw-explore/w2"}
;; <=

;; **
;;; Notice how long it took! This is woefully inefficient. It is a big job - but it isn't that big a job.
;;;
;;; ## Next, visualise that
;;;
;;; I don't have any ready-made code to visualise the waterflow, so I'll have to extemporise. First, we need a way of creating a colour gradient.
;; **

;; @@
(defn shades-of-blue
  "Create CSS specifications for shades of blue which vary in intensity with `n`."
  [n]
  (cond
   (nil? n) "gray"
   (zero? n) "gray"
   (>= n 64) "blue"
   :true (let [intensity (- 255 (* n 4))]
           (str "rgb( " intensity ", " intensity ", 255)"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/shades-of-blue</span>","value":"#'mw-explore.mw-explore/shades-of-blue"}
;; <=

;; @@
(shades-of-blue 100)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;blue&quot;</span>","value":"\"blue\""}
;; <=

;; @@
(shades-of-blue 50)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;rgb( 55, 55, 255)&quot;</span>","value":"\"rgb( 55, 55, 255)\""}
;; <=

;; @@
(shades-of-blue 0)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;gray&quot;</span>","value":"\"gray\""}
;; <=

;; **
;;; ### OK, that works
;;;
;;; Now to build an HTML cell representation of a world cell. Fortunately, I've done something like this before.
;; **

;; @@
(defn render-cell
  "Render this world cell as a Hiccup table cell."
  [cell]
  [:td
   {:style (format "background-color: %s;" (shades-of-blue (or (:flow cell) 0)))
    :title (str cell)}
   "&nbsp;" (:altitude cell)])

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/render-cell</span>","value":"#'mw-explore.mw-explore/render-cell"}
;; <=

;; @@
(render-cell {:flow 10 :altitude 20})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:td</span>","value":":td"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:style</span>","value":":style"},{"type":"html","content":"<span class='clj-string'>&quot;background-color: rgb( 215, 215, 255);&quot;</span>","value":"\"background-color: rgb( 215, 215, 255);\""}],"value":"[:style \"background-color: rgb( 215, 215, 255);\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:title</span>","value":":title"},{"type":"html","content":"<span class='clj-string'>&quot;{:flow 10, :altitude 20}&quot;</span>","value":"\"{:flow 10, :altitude 20}\""}],"value":"[:title \"{:flow 10, :altitude 20}\"]"}],"value":"{:style \"background-color: rgb( 215, 215, 255);\", :title \"{:flow 10, :altitude 20}\"}"},{"type":"html","content":"<span class='clj-string'>&quot;&amp;nbsp;&quot;</span>","value":"\"&nbsp;\""},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"}],"value":"[:td {:style \"background-color: rgb( 215, 215, 255);\", :title \"{:flow 10, :altitude 20}\"} \"&nbsp;\" 20]"}
;; <=

;; @@
(defn render-world-row
  "Render this world `row` as a Hiccup table row."
  [row]
  (apply vector (cons :tr (map render-cell row))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/render-world-row</span>","value":"#'mw-explore.mw-explore/render-world-row"}
;; <=

;; @@
(defn render-world-table
  "Render this `world` as a Hiccup table."
  [world]
  [:html
   [:head
    [:title "Test render"]]
   [:body
    (apply vector
      (cons :table
        (map render-world-row world)))]])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mw-explore.mw-explore/render-world-table</span>","value":"#'mw-explore.mw-explore/render-world-table"}
;; <=

;; **
;;; ### Let's look at how that formats a real cell
;; **

;; @@
(nth (nth w2 128) 128)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:flow</span>","value":":flow"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:flow 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:generation</span>","value":":generation"},{"type":"html","content":"<span class='clj-long'>256</span>","value":"256"}],"value":"[:generation 256]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:rainfall</span>","value":":rainfall"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:rainfall 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:gradient</span>","value":":gradient"},{"type":"html","content":"<span class='clj-long'>68</span>","value":"68"}],"value":"[:gradient 68]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:altitude</span>","value":":altitude"},{"type":"html","content":"<span class='clj-long'>42</span>","value":"42"}],"value":"[:altitude 42]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:x</span>","value":":x"},{"type":"html","content":"<span class='clj-long'>128</span>","value":"128"}],"value":"[:x 128]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"},{"type":"html","content":"<span class='clj-long'>128</span>","value":"128"}],"value":"[:y 128]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:state</span>","value":":state"},{"type":"html","content":"<span class='clj-keyword'>:new</span>","value":":new"}],"value":"[:state :new]"}],"value":"{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}"}
;; <=

;; @@
(render-cell (nth (nth w2 128) 128))

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:td</span>","value":":td"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:style</span>","value":":style"},{"type":"html","content":"<span class='clj-string'>&quot;background-color: rgb( 251, 251, 255);&quot;</span>","value":"\"background-color: rgb( 251, 251, 255);\""}],"value":"[:style \"background-color: rgb( 251, 251, 255);\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:title</span>","value":":title"},{"type":"html","content":"<span class='clj-string'>&quot;{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}&quot;</span>","value":"\"{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}\""}],"value":"[:title \"{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}\"]"}],"value":"{:style \"background-color: rgb( 251, 251, 255);\", :title \"{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}\"}"},{"type":"html","content":"<span class='clj-string'>&quot;&amp;nbsp;&quot;</span>","value":"\"&nbsp;\""},{"type":"html","content":"<span class='clj-long'>42</span>","value":"42"}],"value":"[:td {:style \"background-color: rgb( 251, 251, 255);\", :title \"{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}\"} \"&nbsp;\" 42]"}
;; <=

;; @@
(html (render-cell (nth (nth w2 128) 128)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;&lt;td style=\\&quot;background-color: rgb( 251, 251, 255);\\&quot; title=\\&quot;{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}\\&quot;&gt;&amp;nbsp;42&lt;/td&gt;&quot;</span>","value":"\"<td style=\\\"background-color: rgb( 251, 251, 255);\\\" title=\\\"{:flow 1, :generation 256, :rainfall 1, :gradient 68, :altitude 42, :x 128, :y 128, :state :new}\\\">&nbsp;42</td>\""}
;; <=

;; **
;;; ## OK, now we can visualise what we've got
;;;
;;; We'll render the world we've created to an HTML file, and then look at the file.
;; **

;; @@
(time (spit "render.html" (html (render-world-table w2))))
;; @@
;; ->
;;; &quot;Elapsed time: 2386.180766 msecs&quot;
;;;
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
