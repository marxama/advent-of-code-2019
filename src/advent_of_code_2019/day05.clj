(ns advent-of-code-2019.day05
  (:require [advent-of-code-2019.intcode :as intcode]))

(defn parse-input []
  (intcode/parse-input "resources/day05_input"))

(defn solve-part-1 []
  (let [output (-> (parse-input)
                   (intcode/run-program [1])
                   :output)
        test-codes (butlast output)
        diagnostic-code (last output)]
    (assert (every? zero? test-codes) "Test(s) failed")
    diagnostic-code))

