(ns advent-of-code-2019.day01
  (:require [clojure.string :as string]))

(defn fuel-required [mass]
  (-> mass (/ 3) long (- 2)))

(defn parse-input []
  (->> "resources/day1_input"
       slurp
       string/split-lines
       (map #(Long/parseLong %))))

(defn solution-part-1 []
  (->> (parse-input)
       (map fuel-required)
       (reduce +)))

(defn enhanced-fuel-required [mass]
  (->> mass
       (iterate fuel-required)
       rest ;; Don't include the mass of the module
       (take-while pos?)
       (reduce +)))

(defn solution-part-2 []
  (->> (parse-input)
       (map enhanced-fuel-required)
       (reduce +)))
