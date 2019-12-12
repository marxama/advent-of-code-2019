(ns advent-of-code-2019.day10
  (:require [clojure.string :as string]))

(defn parse-row [row s]
  (->> s
       (map-indexed (fn [column c]
                      (when (= c \#)
                        [column row])))
       (remove nil?)))

(defn parse-string [s]
  (->> s
       string/split-lines
       (mapcat parse-row (range))))

(defn parse-input []
  (-> "resources/day10_input" slurp parse-string))

(defn vec- [a b]
  (map - a b))

(defn vec+ [a b]
  (map + a b))

;; This needs to be bigger than any of the coordinates in play - could be determined
;; during runtime to make it more generic, but this suffices for our needs.
(def ^:const BIG_NUMBER 10000)

(defn sign [x]
  (if (neg? x) -1 1))

(defn poor-mans-normalized-vector
  "Given two vectors that point in the same direction, this function will return
  the same result.

  It's not quite the normalized vector of the passed argument,
  but for our purposes, it fills the same function (but is more efficient)."
  [[x y]]
  (if (zero? x)
    [0 (sign y)]
    (let [denominator (Math/abs x)]
      ; Making y very small here makes it easier to implement the rotational sort order later on
      [(sign x) (/ y denominator BIG_NUMBER)])))

(defn vectors-to-all-asteroids [from asteroids]
  (->> asteroids
       (remove #{from})
       (map #(vec- % from))))

(defn visible-asteroids [from asteroids]
  (->> asteroids
       (vectors-to-all-asteroids from)
       (map poor-mans-normalized-vector)
       distinct))

(defn visible-asteroids-from-best-location [asteroids]
  (->> asteroids
       (map #(visible-asteroids % asteroids))
       (map count)
       (apply max)))

(defn solve-part-1 []
  (visible-asteroids-from-best-location (parse-input)))


(defn length-squared [[x y]]
  (+ (* x x) (* y y)))

(defn vec-rot-sort [[x y]]
  (+ (* BIG_NUMBER (dec y) (sign x))
     (* x BIG_NUMBER -1/2)))

(defn cycle-concat-first
  "Takes a coll-of-colls, and returns a seq with the first item of each coll,
  followed by the second item of each coll, and so on. Note that when a coll
  is emptied in this process, it will be ignored in further cycles - in other
  words, some colls may have many more items than others."
  [coll-of-colls]
  (when (seq coll-of-colls)
    (concat (map first coll-of-colls)
            (lazy-seq (cycle-concat-first (keep next coll-of-colls))))))

(defn get-vaporization-order [from asteroids]
  (->> asteroids
       (vectors-to-all-asteroids from)
       (group-by poor-mans-normalized-vector)
       (sort-by (comp vec-rot-sort first))
       (map val)
       (map #(sort-by length-squared %))
       cycle-concat-first
       (map (partial vec+ from))))

(defn solve-part-2 []
  (->> (parse-input)
       (get-vaporization-order [14 17])
       (drop 199)
       first))
