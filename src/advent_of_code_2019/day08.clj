(ns advent-of-code-2019.day08
  (:require [advent-of-code-2019.utils :as utils]))

(defn parse-input []
  (-> "resources/day08_input" slurp seq))

(defn occurrences [x coll]
  (->> coll
       (filter #{x})
       count))

(defn get-layers [width height coll]
  (partition (* width height) coll))

(defn solve-part-1 []
  (->> (parse-input)
       (get-layers 25 6)
       (apply min-key (partial occurrences \0))
       ((juxt (partial occurrences \1)
              (partial occurrences \2)))
       (apply *)))

(defn get-pixel-color [pixel-layers]
  (->> pixel-layers
       (remove #{\2})
       first))

(defn get-image-colors [layers]
  (let [layer-count (count layers)]
    (->> layers
         (apply interleave)
         (partition layer-count)
         (map get-pixel-color))))

(defn solve-part-2 []
  (->> (parse-input)
       (get-layers 25 6)
       get-image-colors
       (utils/render-image 25 6)))
