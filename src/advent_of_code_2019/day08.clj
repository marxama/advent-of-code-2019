(ns advent-of-code-2019.day08)

(defn parse-input []
  (-> "resources/day08_input" slurp seq))

(defn occurrences [x coll]
  (->> coll
       (filter #{x})
       count))

(defn get-layers [width height coll]
  (partition (* width height) coll))

(defn solve-part-1 []
  (->> (parse-input)
       (get-layers 25 6)
       (apply min-key (partial occurrences \0))
       ((juxt (partial occurrences \1)
              (partial occurrences \2)))
       (apply *)))
