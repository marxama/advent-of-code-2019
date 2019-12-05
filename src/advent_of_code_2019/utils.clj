(ns advent-of-code-2019.utils)

(defn digits [n]
  (if (< n 10)
    [n]
    (concat (digits (quot n 10)) [(mod n 10)])))