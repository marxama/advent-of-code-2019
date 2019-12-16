(ns advent-of-code-2019.day12
  (:require [advent-of-code-2019.math :as math]))

(defn sign [x]
  (cond
    (zero? x) 0
    (neg? x) -1
    :else 1))

(defn apply-gravity [m1 m2]
  (->> (math/vec- (:position m2) (:position m1))
       (map sign)
       (update m1 :velocity math/vec+)))

(defn apply-velocity [{:keys [velocity] :as moon}]
  (update moon :position math/vec+ velocity))

(defn step-moons [moons]
  (->> moons
       (map #(reduce apply-gravity % moons))
       (map apply-velocity)))

(defn step [state]
  (-> state
      (update :steps inc)
      (update :moons step-moons)))

(defn create-moon [[x y z]]
  {:position [x y z]
   :velocity [0 0 0]})

(defn create-initial-state [moon-positions]
  {:moons (map create-moon moon-positions)
   :steps 0})

(defn simulate [steps moons]
  (->> moons
       (iterate step)
       (drop steps)
       first))

(defn sum-of-absolutes [coll]
  (->> coll
       (map #(Math/abs %))
       (reduce +)))

(def kinetic-energy (comp sum-of-absolutes :velocity))

(def potential-energy (comp sum-of-absolutes :position))

(defn total-energy [moon]
  (* (potential-energy moon) (kinetic-energy moon)))

(defn total-system-energy [{:keys [moons]}]
  (->> moons
       (map total-energy)
       (reduce +)))

(defn solve-part-1 []
  (->> [[6 -2 -7]
        [-6 -7 -4]
        [-9 11 0]
        [-3 -4 6]]
       create-initial-state
       (simulate 1000)
       total-system-energy))
