(ns advent-of-code.day6-coordinate-search)

(defn abs [x]
  (if (pos? x)
    x
    (- x)))

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

(def min-x (->> input
                (map first)
                (apply min)))

(def max-x (->> input
                (map first)
                (apply max)))

(def delta-x (- max-x min-x))

(def min-y (->> input
                (map second)
                (apply min)))

(def max-y (->> input
                (map second)
                (apply max)))

(def delta-y (- max-y min-y))

(println min-x max-x delta-x min-y max-y delta-y)

(def distance [coord1 coord2]
  (+ (abs (- (first coord1) (first coord2)))))

(defn part1 [input]
  (let [coordinates (->> input
                         (partition 2))]
    (* ())))



