(ns advent-of-code-2019.day09
  (:require [advent-of-code-2019.intcode :as intcode]))

(defn solve [input]
  (-> "resources/day09_input"
      intcode/parse-input
      intcode/build-program
      (intcode/run-program [input])
      :output))

(defn solve-part-1 []
  (solve 1))

(defn solve-part-2 []
  (solve 2))
