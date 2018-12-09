(ns advent-of-code.day9-marble-game)

(defn circle-to-list [circle]
  (loop [list [0]
         next (second (get circle 0))]
    (if (= 0 next)
      list
      (recur (conj list next)
             (second (get circle next))))))

; a b c add d at b -> a d b c
(defn place-marble
  {:test (fn []
           (clojure.test/is (= (place-marble {0 [2 1]
                                              1 [0 2]
                                              2 [1 0]} 3 1)
                               {0 [2 3]
                                3 [0 1]
                                1 [3 2]
                                2 [1 0]}))
           (clojure.test/is (= (place-marble {0 [1 1]
                                              1 [0 0]} 2 1)
                               {0 [1 2]
                                2 [0 1]
                                1 [2 0]}))
           (clojure.test/is (= (circle-to-list
                                 (place-marble {0 [1 1]
                                                1 [0 0]} 2 1))
                               [0 2 1]))
           (clojure.test/is (= (place-marble {0 [0 0]} 1 0)
                               {1 [0 0]
                                0 [1 1]})))}
  [circle marble-to-place marble-to-push-forward]
  (let [d marble-to-place
        b marble-to-push-forward
        [a c] (get circle b)]
    (-> circle
        (assoc d [a b])
        (assoc b [d c])
        (update a (fn [[x b]] [x d])))))

(defn remove-marble
  {:test (fn []
           (clojure.test/is (= (remove-marble {0 [2 3]
                                               3 [0 1]
                                               1 [3 2]
                                               2 [1 0]} 3)
                               {0 [2 1]
                                1 [0 2]
                                2 [1 0]})))}
  [circle marble-to-remove]
  (let [[before after] (get circle marble-to-remove)]
    (-> circle
        (update before (fn [[x _]] [x after]))
        (update after (fn [[_ x]] [before x]))
        (dissoc marble-to-remove))))

(defn get-marble
  {:test (fn []
           (clojure.test/is (= (get-marble {0 [1 1]
                                            1 [0 0]} 1 2) 1))
           (clojure.test/is (= (get-marble {0 [2 3]
                                            3 [0 1]
                                            1 [3 2]
                                            2 [1 0]} 2 2) 3))
           (clojure.test/is (= (get-marble {0 [2 3]
                                            3 [0 1]
                                            1 [3 2]
                                            2 [1 0]} 2 2) 3)))}
  [circle starting-marble delta]
  (loop [current-marble starting-marble
         delta          delta]
    (cond
      (= delta 0)
      current-marble
      (> delta 0)
      (recur (second (get circle current-marble))
             (dec delta))
      (< delta 0)
      (recur (first (get circle current-marble))
             (inc delta)))))

(def last-marble-worth 7202600)
(def n-players 471)

(defn part1
  {:test (fn []
           (clojure.test/is (= (part1 26 9)
                               32))
           (clojure.test/is (= (part1 72026 471)
                               390093)))}
  [last-marble-worth
   n-players]
  (->> (loop [circle         {0 [0 0]}
              current-marble 0
              marble-to-play 1
              current-player 1
              scores         {}]
         (when (= 0 (mod marble-to-play 10000)) (println marble-to-play))
         (cond
           (> marble-to-play last-marble-worth)
           scores
           (= 0 (mod marble-to-play 23))
           (let [marble-to-remove (get-marble circle current-marble -7)
                 marble-removed   marble-to-remove
                 score            (+ marble-removed marble-to-play)]
             (recur (remove-marble circle marble-to-remove)
                    (second (get circle marble-to-remove))
                    (inc marble-to-play)
                    (mod (inc current-player) n-players)
                    (update scores current-player (fn [existing-score] (+ score (or existing-score 0))))))
           :default
           (let [next-marble-position (get-marble circle current-marble 2)]
             (recur (place-marble circle marble-to-play next-marble-position)
                    marble-to-play
                    (inc marble-to-play)
                    (mod (inc current-player) n-players)
                    scores))))
       (map second)
       (apply max)))
