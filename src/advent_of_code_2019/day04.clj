(ns advent-of-code-2019.day04)

(defn input-range []
  (range 272091 815433))

(defn digits [n]
  (if (< n 10)
    [n]
    (concat (digits (quot n 10)) [(mod n 10)])))

(defn non-decreasing? [coll]
  (apply <= coll))

(defn has-adjacent-duplicates? [coll]
  (->> coll
       (partition 2 1)
       (some (partial apply =))))

(def meets-criteria?
  (comp (every-pred non-decreasing? has-adjacent-duplicates?) digits))

(defn solve-part-1 []
  (->> (input-range)
       (filter meets-criteria?)
       count))

(defn partition-into-adjacents [coll]
  (partition-by identity coll))

(defn has-adjacent-pairs? [coll]
  (some (comp #{2} count) (partition-into-adjacents coll)))

(def meets-criteria-2?
  (comp (every-pred non-decreasing? has-adjacent-pairs?) digits))

(defn solve-part-2 []
  (->> (input-range)
       (filter meets-criteria-2?)
       count))
