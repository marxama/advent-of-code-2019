(ns advent-of-code-2019.math)

(defn vec+ [v1 v2]
  (mapv + v1 v2))

(defn vec- [v1 v2]
  (mapv - v1 v2))

(defn mat*vec [m v]
  (mapv (fn [row] (apply + (map * row v))) m))
