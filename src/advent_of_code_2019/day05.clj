(ns advent-of-code-2019.day05
  (:require [advent-of-code-2019.intcode :as intcode]))

(defn parse-input []
  (intcode/parse-input "resources/day05_input"))

(defn solve [id]
  (let [output (-> (parse-input)
                   intcode/build-program
                   (intcode/run-program [id])
                   :output)
        test-codes (butlast output)
        diagnostic-code (last output)]
    (assert (every? zero? test-codes) "Test(s) failed")
    diagnostic-code))

(defn solve-part-1 []
  (solve 1))

(defn solve-part-2 []
  (solve 5))