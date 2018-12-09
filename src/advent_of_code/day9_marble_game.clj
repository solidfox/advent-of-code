(ns advent-of-code.day9-marble-game)

(defn place-marble
  {:test (fn []
           (clojure.test/is (= (place-marble [0 1 2] 3 1)
                               [0 3 1 2]))
           (clojure.test/is (= (place-marble [0 1 2] 3 3)
                               [0 1 2 3]))
           (clojure.test/is (= (place-marble [0 1] 2 1)
                               [0 2 1])))}
  [circle marble-to-place position]
  (let [[before after] (split-at position
                                 circle)]
    (vec (concat before [marble-to-place] after))))

(defn remove-marble
  {:test (fn []
           (clojure.test/is (= (remove-marble [0 1 2] 2) [0 1]))
           (clojure.test/is (= (remove-marble [0 1 2] 0) [1 2])))}
  [circle position-to-remove]
  (let [[before after] (split-at position-to-remove circle)]
    (vec (concat before (drop 1 after)))))

(defn get-circle-position
  {:test (fn []
           (clojure.test/is (= (get-circle-position [0 1 2 3 4 5 6 7] 7 -7) 0))
           (clojure.test/is (= (get-circle-position [0 1] 1 2) 1)))}
  [circle position delta]
  (-> position
      (+ delta)
      (mod (count circle))))

(def last-marble-worth 72026)
(def n-players 471)

(defn part1
  {:test (fn [] (clojure.test/is (= (part1 25 9)
                                    32)))}
  [last-marble-worth
   n-players]
  (->> (loop [circle                  [0]
              current-marble-position 0
              marble-to-play          1
              current-player          1
              scores                  {}]
         (when (= 0 (mod marble-to-play 100)) (println marble-to-play))
         (cond
           (> marble-to-play last-marble-worth)
           [scores circle]
           (= 0 (mod marble-to-play 23))
           (let [position-to-remove (get-circle-position circle current-marble-position -7)
                 marble-removed     (nth circle position-to-remove)
                 score              (+ marble-removed marble-to-play)]
             (recur (remove-marble circle position-to-remove)
                    position-to-remove
                    (inc marble-to-play)
                    (mod (+ 1 current-player) n-players)
                    (update scores current-player (fn [existing-score] (+ score (or existing-score 0))))))
           :default
           (let [next-marble-position (as-> (get-circle-position circle current-marble-position 2) $
                                            (if (= $ 0) (count circle) $))]

             (recur (place-marble circle marble-to-play next-marble-position)
                    next-marble-position
                    (inc marble-to-play)
                    (mod (+ 1 current-player) n-players)
                    scores))))
       (first)
       (map second)
       (apply max)))
