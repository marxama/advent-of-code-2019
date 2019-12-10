(ns advent-of-code-2019.intcode
  (:require [clojure.string :as string]
            [advent-of-code-2019.utils :as utils]))

(defn parse-input [f]
  (-> f
      slurp
      (string/split #",")
      (->> (map #(Integer/parseInt %))
           vec)))

(defn increase-memory-to-fit [memory size]
  (if (< (count memory) size)
    (into memory (repeat (- size (count memory)) 0))
    memory))

(def ^:const POSITION_MODE 0)
(def ^:const IMMEDIATE_MODE 1)
(def ^:const RELATIVE_MODE 2)

(defn get-memory-location [{:keys [relative-base]} [value mode]]
  (condp = mode
    POSITION_MODE value
    IMMEDIATE_MODE nil
    RELATIVE_MODE (+ relative-base value)))

(defn get-value [{:keys [memory] :as state} [value mode :as parameter]]
  (or (if (= mode IMMEDIATE_MODE)
        value
        (get memory (get-memory-location state parameter)))
      0))

(defn store-value [state location-parameter value]
  (let [location (get-memory-location state location-parameter)]
    (-> state
        (update :memory increase-memory-to-fit location)
        (assoc-in [:memory location] value))))

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
      :f (fn [{[input & remaining-input] :input :as state} a]
           (if input
             (-> state
                 (store-value a input)
                 (assoc :input remaining-input))
             (assoc state :awaiting-input? true)))}

   4 {:operand-count 1
      :f (fn [state a]
           (update state :output conj (get-value state a)))}

   5 {:operand-count 2
      :manually-updates-pc? true
      :f (fn [state a b]
           (if-not (zero? (get-value state a))
             (assoc state :pc (get-value state b))
             (update state :pc + 3)))}

   6 {:operand-count 2
      :manually-updates-pc? true
      :f (fn [state a b]
           (if (zero? (get-value state a))
             (assoc state :pc (get-value state b))
             (update state :pc + 3)))}

   7 {:operand-count 3
      :f (fn [state a b c]
           (store-value state c
                        (if (< (get-value state a) (get-value state b))
                          1
                          0)))}

   8 {:operand-count 3
      :f (fn [state a b c]
           (store-value state c
                        (if (= (get-value state a) (get-value state b))
                          1
                          0)))}

   9 {:operand-count 1
      :f (fn [state a]
           (update state :relative-base + (get-value state a)))}

   99 {:operand-count 0
       :f #(assoc % :terminated? true)}})

(defn parse-current-instruction [{:keys [memory pc]}]
  (let [[opcode-1s opcode-10s & modes] (reverse (utils/digits (get memory pc)))
        opcode (+ opcode-1s (* 10 (or opcode-10s 0)))]
    [opcode modes]))

(defn step [{:keys [pc memory] :as state}]
  (let [[opcode parameter-modes] (parse-current-instruction state)
        {:keys [operand-count f manually-updates-pc?]} (instructions opcode)
        operand-values (->> (range operand-count)
                            (map #(+ % pc 1))
                            (map #(get memory %)))
        operands (partition 2 (interleave operand-values (concat parameter-modes (repeat 0))))
        {:keys [awaiting-input? terminated?] :as new-state} (apply f state operands)]
    (cond-> new-state
      (not (or manually-updates-pc? awaiting-input? terminated?)) (update :pc + 1 operand-count))))

(defn build-program [memory]
  {:memory memory
   :pc 0
   :relative-base 0
   :output []})

(defn run-program
  ([program]
   (run-program program []))
  ([program input]
   (->> (assoc program
               :input input
               :awaiting-input? false)
        (iterate step)
        (drop-while (complement (some-fn :terminated? :awaiting-input?)))
        first)))
