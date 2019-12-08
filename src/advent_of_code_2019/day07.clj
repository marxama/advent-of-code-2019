(ns advent-of-code-2019.day07
  (:require [advent-of-code-2019.intcode :as intcode]
            [advent-of-code-2019.utils :as utils]))

(defn run-amplifier-sequence [program phase-settings]
  (->> phase-settings
       (reduce (fn [{[output] :output} phase-setting]
                 (intcode/run-program program [phase-setting output]))
               {:output [0]})
       :output
       first))

(defn solve-part-1 []
  (let [program (intcode/build-program (intcode/parse-input "resources/day07_input"))]
    (->> (range 5)
         utils/permutations
         (map #(run-amplifier-sequence program %))
         (apply max))))
