(ns advent-of-code.day12-plant-school
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input "initial state: ##.......#.######.##..#...#.#.#..#...#..####..#.##...#....#...##..#..#.##.##.###.##.#.......###....#\n\n.#### => .\n....# => .\n###.. => .\n..#.# => .\n##### => #\n####. => .\n#.##. => #\n#.#.# => .\n##.#. => #\n.###. => .\n#..#. => #\n###.# => .\n#.### => .\n##... => #\n.#.## => .\n..#.. => .\n#...# => #\n..... => .\n.##.. => .\n...#. => .\n#.#.. => .\n.#..# => #\n.#.#. => .\n.#... => #\n..##. => .\n#..## => .\n##.## => #\n...## => #\n..### => #\n#.... => .\n.##.# => #\n##..# => #")

(defn parse-rules [input]
  (->> input
       (str/split-lines)
       (map (fn [line]
              (first (drop 1 (re-matches #"(.....) => #" line)))))
       (remove empty?)))

(defn parse-initial-state [input]
  (->> input
       (str/split-lines)
       (first)
       (re-matches #"^initial state: (.+)")
       (drop 1)
       (first)
       (map vector (range 0 400))
       (filter (fn [[_ plant?]] (= plant? \#)))
       (map first)
       (into #{})))

(defn spy [x] (println x) x)

(defn perform-rules
  {:test (fn [] (is (= (perform-rules (parse-initial-state "initial state: #..") #{".#..."
                                                                                   "...#."
                                                                                   "..#.."})
                       #{-1 0 1})))}
  [state rules]
  (let [plantless-of-interest (->> (map (fn [pot-index]
                                          (range (- pot-index 4) (+ pot-index 5)))
                                        state)
                                   (apply concat)
                                   (remove (partial contains? state)))
        all-of-interest (concat state plantless-of-interest)]
    (->> all-of-interest
         (filter (fn [pot-index]
                   (->> (range (- pot-index 2) (+ pot-index 3))
                        (map (fn [pot-index] (if (contains? state pot-index) \# \.)))
                        (apply str)
                        (contains? rules))))
         (into #{}))))

(defn part1 [input]
  (time (let [rules (into #{} (parse-rules input))
              initial-state (parse-initial-state input)]
          (->> (reduce (fn [state iteration]
                         (perform-rules state rules))
                       initial-state
                       (range 0 20))
               (reduce +)))))

(defn part2 [input]
  (time (let [rules (into #{} (parse-rules input))
              initial-state (parse-initial-state input)]
          (->> (reduce (fn [state iteration]
                         (println)
                         (println iteration
                                  (apply min state)
                                  (apply max state)
                                  (reduce + state))
                         (perform-rules state rules))
                       initial-state
                       (range 0 1000))))))
