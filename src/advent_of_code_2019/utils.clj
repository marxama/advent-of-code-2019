(ns advent-of-code-2019.utils)

(defn digits [n]
  (if (< n 10)
    [n]
    (concat (digits (quot n 10)) [(mod n 10)])))

;; TODO: Surely there's a prettier way of doing this...
(defn permutations
  "Returns a seq of all permutations of coll. Note that equality of items does not matter, e.g.
  duplicates are treated as distinct items."
  [coll]
  (when (seq coll)
    (let [count (count coll)
          cycled (->> coll cycle (take (* 2 count)) vec)]
      (->> count
           range
           (mapcat (fn [index]
                     (let [[x & r] (->> cycled
                                        (drop index)
                                        (take count))]
                       (if (seq r)
                         (map #(cons x %) (permutations r))
                         [[x]]))))))))

(defn render-image [width height pixels]
  (doseq [row (partition width pixels)]
    (doseq [pixel row]
      (print (if (= pixel \0) \⬛ \⬜)))
    (println)))
