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

(defn create-vector [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y1 y2)])

(defn poor-mans-normalized-vector
  "Given two vectors that point in the same direction, this function will return
  the same result.

  It's not quite the normalized vector of the passed argument,
  but for our purposes, it fills the same function (but is more efficient)."
  [[x y]]
  (let [denominator (Math/abs (if (zero? x) y x))]
    [(/ x denominator) (/ y denominator)]))

(defn vectors-to-all-asteroids [from asteroids]
  (->> asteroids
       (remove #{from})
       (map #(zipmap [:pos :vec] [% (create-vector from %)]))))

(defn visible-asteroids [from asteroids]
  (->> asteroids
       (vectors-to-all-asteroids from)
       (map :vec)
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

(defn sign [x]
  (if (neg? x) -1 1))

;; THERE HAS TO BE A BETTER WAY ARGH
(defn vec< [[x1 y1 :as a] [x2 y2 :as b]]
  (cond
    (= a b) false
    (and (zero? x1) (zero? x2)) (> y1 y2)
    (and (zero? y1) (zero? y2)) (> x1 x2)
    (zero? x1) (or (pos? y1)
                   (neg? x2))
    (zero? x2) (and (neg? y2)
                    (pos? x1))
    (and (pos? x1) (pos? x2)) (> y1 y2)
    (pos? x1) true
    (pos? x2) false
    :else (< y1 y2)))

(defn cycle-flatten
  "Takes a coll-of-colls, and returns a seq with the first item of each coll,
  followed by the second item of each coll, and so on. Note that when a coll
  is emptied in this process, it will be ignored in further cycles - in other
  words, some colls may have many more items than others."
  [coll-of-colls]
  (loop [coll-of-colls coll-of-colls
         result ()]
    (if (seq coll-of-colls)
      (recur
       (keep next coll-of-colls)
       (concat result (map first coll-of-colls)))
      result)))

(defn get-vaporization-order [from asteroids]
  (->> asteroids
       (vectors-to-all-asteroids from)
       (group-by (comp poor-mans-normalized-vector :vec))
       (sort-by first (comparator vec<))
       (map val)
       (map #(sort-by (comp length-squared :vec) %))
       cycle-flatten))

(defn solve-part-2 []
  (->> (parse-input)
       (get-vaporization-order [14 17])
       (drop 199)
       first
       :pos))
