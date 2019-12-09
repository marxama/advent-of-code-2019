(ns advent-of-code-2019.day07
  (:require [advent-of-code-2019.intcode :as intcode]
            [advent-of-code-2019.utils :as utils]))

(defn build-amplifiers [program]
  (->> program intcode/build-program (repeat 5)))

(defn run-amplifier-sequence
  [amplifiers initial-input phase-settings]
  (->> (interleave amplifiers phase-settings)
       (partition 2)
       (reductions (fn [{output :output} [amplifier phase-setting]]
                     (intcode/run-program amplifier (if phase-setting
                                                      (cons phase-setting output)
                                                      output)))
                   {:output initial-input})
       (drop 1)))

(defn get-last-amplifier-output [amplifiers]
  (-> amplifiers last :output first))

(defn solve-part-1 []
  (let [amplifiers (build-amplifiers (intcode/parse-input "resources/day07_input"))]
    (->> (range 5)
         utils/permutations
         (map #(run-amplifier-sequence amplifiers [0] %))
         (map get-last-amplifier-output)
         (apply max))))

(defn run-feedback-cycle [amplifiers initial-input phase-settings]
  (let [new-amplifiers (run-amplifier-sequence amplifiers initial-input phase-settings)]
    (if (-> new-amplifiers last :terminated?)
      (get-last-amplifier-output new-amplifiers)
      (recur (map #(assoc % :output []) new-amplifiers)
             (-> new-amplifiers last :output)
             (repeat nil)))))

(defn solve-part-2
  ([] (solve-part-2 (intcode/parse-input "resources/day07_input")))
  ([program]
   (let [amplifiers (build-amplifiers program)]
     (->> (range 5 10)
          utils/permutations
          (map #(run-feedback-cycle amplifiers [0] %))
          (apply max)))))
