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
  (let [orbit-lookup (get-orbit-lookup (parse-input))]
    (->> (keys orbit-lookup)
         (map #(get-path-to-root orbit-lookup %))
         (map (comp dec count))
         (reduce +))))

(defn solve-part-2 []
  (let [orbit-lookup (get-orbit-lookup (parse-input))]
    (->> ["YOU" "SAN"]
         (map #(get-path-to-root orbit-lookup %))
         (map rest) ; Do not include "YOU" and "SAN" themselves
         (map set)
         (apply data/diff)
         (take 2) ; take distinct objects in YOU and SAN paths
         (map count)
         (reduce +))))
