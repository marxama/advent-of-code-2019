(ns advent-of-code-2019.day02
  (:require [clojure.string :as string]))

(defn parse-input []
  (-> "resources/day02_input"
      slurp
      (string/split #",")
      (->> (map #(Integer/parseInt %))
           vec)))

(defn value-referenced-at [memory address]
  (get memory (get memory address)))

(defmulti step (fn [{:keys [memory pc]}] (nth memory pc)))

(defn step-arithmetic [{:keys [memory pc] :as state} f]
  (-> state
      (assoc-in [:memory (get memory (+ pc 3))]
                (f (value-referenced-at memory (+ pc 1))
                   (value-referenced-at memory (+ pc 2))))
      (assoc :pc (+ pc 4))))

;; Addition
(defmethod step 1 [state]
  (step-arithmetic state +))

;; Multiplication
(defmethod step 2 [{:keys [memory pc] :as state}]
  (step-arithmetic state *))

;; Terminate
(defmethod step 99 [state]
  (assoc state :terminated? true))

(defn run-program [program]
  (->> {:memory program
        :pc 0}
       (iterate step)
       (drop-while (complement :terminated?))
       first))

(defn set-parameters [program a b]
  (assoc program
         1 a
         2 b))

(defn solution-part-1 []
  (-> (parse-input)
      (set-parameters 12 2)
      run-program
      :memory
      first))

(defn solution-part-2 []
  (let [program (parse-input)
        
        {:keys [noun verb]}
        (->> (for [noun (range 100)
                   verb (range 100)]
               {:noun noun
                :verb verb
                :result (-> program
                            (set-parameters noun verb)
                            run-program
                            :memory
                            first)})
             (filter #(-> % :result (= 19690720)))
             first)]
    (+ (* 100 noun) verb)))

