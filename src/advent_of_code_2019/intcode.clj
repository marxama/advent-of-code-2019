(ns advent-of-code-2019.intcode
  (:require [clojure.string :as string]
            [advent-of-code-2019.utils :as utils]))

(defn parse-input [f]
  (-> f
      slurp
      (string/split #",")
      (->> (map #(Integer/parseInt %))
           vec)))

(defn store-value [state [location] value]
  (assoc-in state [:memory location] value))

(def ^:const POSITION_MODE 0)
(def ^:const IMMEDIATE_MODE 1)

(defn get-value [{:keys [memory]} [value mode]]
  (condp = mode
    POSITION_MODE (get memory value)
    IMMEDIATE_MODE value))

(def instructions
  {1 {:operand-count 3
      :f (fn [state a b c]
           (store-value state c
                        (+ (get-value state a)
                           (get-value state b))))}

   2 {:operand-count 3
      :f (fn [state a b c]
           (store-value state c
                        (* (get-value state a)
                           (get-value state b))))}

   3 {:operand-count 1
      :f (fn [{:keys [input] :as state} a]
           (-> state
               (store-value a (first input))
               (update :input rest)))}

   4 {:operand-count 1
      :f (fn [state a]
           (update state :output conj (get-value state a)))}

   99 {:operand-count 0
       :f #(assoc % :terminated? true)}})

(defn parse-current-instruction [{:keys [memory pc]}]
  (let [[opcode-1s opcode-10s & modes] (reverse (utils/digits (get memory pc)))
        opcode (+ opcode-1s (* 10 (or opcode-10s 0)))]
    [opcode modes]))

(defn step [{:keys [pc memory] :as state}]
  (let [[opcode parameter-modes] (parse-current-instruction state)
        {:keys [operand-count f]} (instructions opcode)
        operand-values (->> (range operand-count) (map #(+ % pc 1)) (map #(get memory %)))
        operands (partition 2 (interleave operand-values (concat parameter-modes (repeat 0))))]
    (-> (apply f state operands)
        (update :pc + 1 operand-count))))

(defn run-program
  ([program]
   (run-program program []))
  ([program input]
   (->> {:memory program
         :pc 0
         :input input
         :output []}
        (iterate step)
        (drop-while (complement :terminated?))
        first)))
