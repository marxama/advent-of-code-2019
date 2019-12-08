(ns advent-of-code-2019.day02
  (:require [advent-of-code-2019.intcode :as intcode]))

(defn parse-input []
  (intcode/parse-input "resources/day02_input"))

(defn set-parameters [program a b]
  (assoc program
         1 a
         2 b))

(defn solve-part-1 []
  (-> (parse-input)
      (set-parameters 12 2)
      intcode/build-program
      intcode/run-program
      :memory
      first))

(defn solve-part-2 []
  (let [program (parse-input)

        {:keys [noun verb]}
        (->> (for [noun (range 100)
                   verb (range 100)]
               {:noun noun
                :verb verb
                :result (-> program
                            (set-parameters noun verb)
                            intcode/build-program
                            intcode/run-program
                            :memory
                            first)})
             (filter #(-> % :result (= 19690720)))
             first)]
    (+ (* 100 noun) verb)))

