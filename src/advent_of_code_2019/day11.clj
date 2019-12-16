(ns advent-of-code-2019.day11
  (:require [advent-of-code-2019.intcode :as intcode]
            [advent-of-code-2019.math :as math]
            [advent-of-code-2019.utils :as utils]))

(defn parse-input []
  (intcode/parse-input "resources/day11_input"))

(defn turn-matrix [new-direction]
  (if (zero? new-direction)
    [[0 -1]
     [1 0]]
    [[0  1]
     [-1 0]]))

(defn turn [current-direction new-direction]
  (math/mat*vec (turn-matrix new-direction) current-direction))

(defn move [current-location direction]
  (math/vec+ current-location direction))

(defn step [{:keys [direction location visited-locations program] :as state}]
  (let [current-color (visited-locations location 0)
        {[new-color turn-direction] :output :as new-program} (intcode/run-program program [current-color])
        new-direction (turn direction turn-direction)]
    (-> state
        (assoc :program (update new-program :output empty))
        (assoc :direction new-direction)
        (update :location move new-direction)
        (assoc-in [:visited-locations location] new-color))))

(defn run-robot [program start-color]
  (let [state {:direction [0 1]
               :location [0 0]
               :visited-locations {[0 0] start-color}
               :program (intcode/build-program program)}]
    (->> state
         (iterate step)
         (drop-while (complement (comp :terminated? :program)))
         first)))

(defn solve-part-1 []
  (-> (parse-input)
      (run-robot 0)
      :visited-locations
      count))

(defn render [locations]
  (let [x-coords (map first (keys locations))
        y-coords (map second (keys locations))
        min-x (apply min x-coords)
        min-y (apply min y-coords)
        width (inc (- (apply max x-coords) min-x))
        height (inc (- (apply max y-coords) min-y))]
    (->> (for [row (reverse (map (partial + min-y) (range height)))
               col (map (partial + min-x) (range width))]
           (-> (locations [col row] 0) str first))
         (utils/render-image width height))))

(defn solve-part-2 []
  (-> (parse-input)
      (run-robot 1)
      :visited-locations
      render))
