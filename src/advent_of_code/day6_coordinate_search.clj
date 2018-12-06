(ns advent-of-code.day6-coordinate-search
  (:require [clojure.test :refer [is]]))

(defn abs [x]
  (if (pos? x)
    x
    (- x)))

(defn x [point] (first point))

(defn y [point] (second point))

(def input
  (partition 2 [311, 74
                240, 84
                54, 241
                99, 336
                53, 244
                269, 353
                175, 75
                119, 271
                267, 301
                251, 248
                216, 259
                327, 50
                120, 248
                56, 162
                42, 278
                309, 269
                176, 74
                305, 86
                93, 359
                311, 189
                85, 111
                255, 106
                286, 108
                233, 228
                105, 211
                66, 256
                213, 291
                67, 53
                308, 190
                320, 131
                254, 179
                338, 44
                88, 70
                296, 113
                278, 75
                92, 316
                274, 92
                147, 121
                71, 181
                113, 268
                317, 53
                188, 180
                42, 267
                251, 98
                278, 85
                268, 266
                334, 337
                74, 69
                102, 227
                194, 239]))

(defn min-x [input] (->> input
                         (map first)
                         (apply min)))

(defn max-x [input] (->> input
                         (map first)
                         (apply max)))

(defn min-y [input] (->> input
                         (map second)
                         (apply min)))

(defn max-y [input] (->> input
                         (map second)
                         (apply max)))

(defn distance
  {:test (fn []
           (is (= (distance [1 0] [0 0]) 1))
           (is (= (distance [0 0] [1 0]) 1))
           (is (= (distance [1 1] [2 2]) 2))
           (is (= (distance [2 2] [1 1]) 2)))}
  [coord1 coord2]
  (+ (abs (- (first coord1) (first coord2)))
     (abs (- (second coord1) (second coord2)))))

(def field
  (for [x (range (min-x input) (max-x input))
        y (range (min-y input) (max-y input))]
    [x y]))

(defn closest-point [field-point candidates]
  (let [top-two-candidates-and-distances (->> candidates
                                              (map (fn [candidate]
                                                     {:point    candidate
                                                      :distance (distance field-point candidate)}))
                                              (sort-by :distance)
                                              (take 2))]
    (if (not= (:distance (first top-two-candidates-and-distances))
              (:distance (second top-two-candidates-and-distances)))
      (:point (first top-two-candidates-and-distances)))))

(defn point-out-of-bounds?
  {:test (fn []
           (is (point-out-of-bounds? [42 100] input))
           (is (point-out-of-bounds? [100 44] input))
           (is (point-out-of-bounds? [338 100] input))
           (is (point-out-of-bounds? [100 359] input))
           (is (not (point-out-of-bounds? [43 100] input)))
           (is (not (point-out-of-bounds? [100 45] input)))
           (is (not (point-out-of-bounds? [337 100] input)))
           (is (not (point-out-of-bounds? [100 358] input))))}
  [point input]
  (or (<= (x point) (min-x input))
      (>= (x point) (max-x input))
      (<= (y point) (min-y input))
      (>= (y point) (max-y input))))

(defn part1 ;5035
  [input field]
  (->> field
       (pmap (fn [field-point]
               [field-point (closest-point field-point input)]))
       (group-by second)
       (map (fn [[point closest-points-dirty]]
              [point (map first closest-points-dirty)]))
       (remove (fn [[point closest-points]]
                 (some (fn [point] (point-out-of-bounds? point input)) closest-points)))
       (sort-by (comp count second))
       (reverse)
       (take 3)
       (map (comp count second))))

(defn sum-distances-between-points
  [point points]
  (->> points
       (map (partial distance point))
       (reduce +)))

(defn part2 ;35294
  [input field]
  (->> field
       (pmap (fn [field-point]
               [field-point (sum-distances-between-points field-point input)]))
       (filter (fn [[field-point distance-sum]]
                 (< distance-sum 10000)))
       (count)))




