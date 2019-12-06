(ns advent-of-code-2019.day06
  (:require [clojure.string :as string]
            [clojure.data :as data]))

(defn parse-input []
  (->> "resources/day06_input"
       slurp
       string/split-lines
       (map #(string/split % #"\)"))))

(defn get-path-to-root [orbit-lookup x]
  (and x (cons x (get-path-to-root orbit-lookup (orbit-lookup x)))))

(defn get-orbit-lookup [orbits]
  (->> orbits
       (map (fn [[orbited orbiteer]] {orbiteer orbited}))
       (apply merge)))

(defn solve-part-1 []
  (let [orbit-lookup (get-orbit-lookup (parse-input))
        objects (keys orbit-lookup)]
    (->> objects
         (map #(get-path-to-root orbit-lookup %))
         (map (comp dec count))
         (reduce +))))

(defn solve-part-2 []
  (let [orbit-lookup (get-orbit-lookup (parse-input))
        [you-objects san-objects] (->> ["YOU" "SAN"]
                                       (map #(get-path-to-root orbit-lookup %))
                                       (map rest)
                                       (map set))
        [distinct-you-objects distinct-san-objects] (data/diff you-objects san-objects)]
    (+ (count distinct-you-objects) (count distinct-san-objects))))
