(ns advent-of-code-2019.day03
  (:require [clojure.string :as string]
            [clojure.data :refer [diff]]))

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn parse-command [s]
  {:fs (case (.substring s 0 1)
         "U" [identity inc]
         "D" [identity dec]
         "L" [dec identity]
         "R" [inc identity])
   :steps (Long/parseLong (.substring s 1))})

(defn parse-commands [s]
  (map parse-command (string/split s #",")))

(defn single-step [[xf yf] [x y]]
  [(xf x) (yf y)])

(defn apply-command [pos {:keys [fs steps]}]
  (rest (take (inc steps) (iterate (partial single-step fs) pos))))

(defn walk [commands]
  (reduce (fn [path command]
            (into path (apply-command (last path) command)))
          [[0 0]]
          commands))

(defn intersections [path-1 path-2]
  (-> (diff (set path-1) (set path-2))
      (nth 2)
      (disj [0 0])))

(defn parse-input []
  (->> "resources/day03_input"
       slurp
       string/split-lines
       (map parse-commands)))

(defn solve-part-1 []
  (->> (parse-input)
       (map walk)
       (apply intersections)
       (map distance)
       (apply min)))

(defn path-to-distance-lookup [path]
  (->> path
       (map-indexed (fn [distance pos] {pos distance}))
       (apply merge-with (fn [a _] a))))

(defn solve-part-2 []
  (let [paths (map walk (parse-input))
        distances (map path-to-distance-lookup paths)]
    (->> (apply intersections paths)
         (map #(apply + ((apply juxt distances) %))) ; wat
         (apply min))))
