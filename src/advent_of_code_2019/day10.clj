(ns advent-of-code-2019.day10
  (:require [clojure.string :as string]))

(defn parse-row [row s]
  (->> s
       seq
       (map-indexed (fn [column c]
                      (if (= c \#)
                        [column row]
                        nil)))
       (remove nil?)))

(defn parse-string [s]
  (->> s
       string/split-lines
       (mapcat parse-row (range))))

(defn parse-input []
  (-> "resources/day10_input" slurp parse-string))

(defn create-vector [from to]
  (map - to from))

(defn poor-mans-normalized-vector
  "Given two vectors that point in the same direction, this function will return
  the same result.

  It's not quite the normalized vector of the passed argument,
  but for our purposes, it fills the same function (but is more efficient)."
  [[x y]]
  (let [denominator (Math/abs (if (zero? x) y x))]
    [(/ x denominator) (/ y denominator)]))

(defn visible-asteroids [from asteroids]
  (->> asteroids
       (map (partial create-vector from))
       (remove (partial every? zero?))
       (map poor-mans-normalized-vector)
       distinct
       count))

(defn visible-asteroids-from-best-location [asteroids]
  (->> asteroids
       (map #(visible-asteroids % asteroids))
       (apply max)))

(defn solve-part-1 []
  (visible-asteroids-from-best-location (parse-input)))