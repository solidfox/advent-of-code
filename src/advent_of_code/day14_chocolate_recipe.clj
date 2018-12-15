(ns advent-of-code.day14-chocolate-recipe
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(def state {:recipes      [3 7]
            :elf-a-recipe 0
            :elf-b-recipe 1})

(def target-n-recipes 440231)
(def target-sequence "440231")
;(def target-n-recipes 10)

(defn get-elf-a-recipe [state]
  (get-in state [:recipes (:elf-a-recipe state)]))

(defn get-elf-b-recipe [state]
  (get-in state [:recipes (:elf-b-recipe state)]))

(defn combine-recipes
  [state]
  (update state :recipes
          (fn [recipes]
            (reduce conj
                    recipes
                    (->> (+ (get-elf-a-recipe state)
                            (get-elf-b-recipe state))
                         (str)
                         (map str)
                         (map read-string))))))

(defn spy [x] (println x) x)

(defn move-to-new-recipes [state]
  (-> state
      (update :elf-a-recipe (fn [recipe-index] (mod (+ recipe-index (get-elf-a-recipe state) 1) (count (:recipes state)))))
      (update :elf-b-recipe (fn [recipe-index] (mod (+ recipe-index (get-elf-b-recipe state) 1) (count (:recipes state)))))))

(defn part-1
  {:test (fn [] (is (= (part-1 state 10)
                       [1 5 8 9 1 6 7 7 9 2])))}
  [state target-n-recipes]
  (loop [state state]
    (if (>= (count (:recipes state)) (+ 10 target-n-recipes))
      (->> (:recipes state)
           (drop target-n-recipes)
           (take 10))
      (recur (-> state
                 (combine-recipes)
                 (move-to-new-recipes))))))

(defn part-2
  {:test (fn [] (is (= (part-2 state "77")
                       16)))}
  [state target-sequence]
  (loop [state state]
    (let [n-recipes (count (:recipes state))]
      (when (= 0 (mod n-recipes 10000)) (println n-recipes))
      (if (and (> n-recipes (+ 3 (count target-sequence)))
               (str/index-of (str/join (subvec (:recipes state)
                                               (- n-recipes (count target-sequence) 3)))
                             target-sequence))
        (do (println "match! " (count (:recipes state)))
            (-> (:recipes state)
                (str/join)
                (str/split (re-pattern target-sequence))
                (first)
                (count)))
        (recur (-> state
                   (combine-recipes)
                   (move-to-new-recipes)))))))
