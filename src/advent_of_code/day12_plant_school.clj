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
       (map vector (range 0 400))))

(defn spy [x] (println x) x)

(defn prepare-space
  {:test (fn [] (is (= (prepare-space [[1 \.]])
                       [[-3 \.] [-2 \.] [-1 \.] [0 \.] [1 \.] [2 \.] [3 \.] [4 \.] [5 \.]])))}
  [state]
  (let [first-pot-index (ffirst state)
        last-pot-index (first (last state))]
    (concat (map vector (range (- first-pot-index 4) first-pot-index) (repeat \.))
            state
            (map vector (range (+ last-pot-index 1) (+ last-pot-index 5)) (repeat \.)))))

(defn perform-rules
  {:test (fn [] (is (= (perform-rules (map vector (range 1 6) [\.\.\#\.\.]) #{"..#.."}))))}
  [state rules]
  (map (fn [a b c d e]
         (let [string (apply str (map second [a b c d e]))]
           (if (contains? rules string)
             [(first c) \#]
             [(first c) \.])))
       state
       (drop 1 state)
       (drop 2 state)
       (drop 3 state)
       (drop 4 state)))

(defn part1 [input]
  (time (let [rules (into #{} (parse-rules input))
              initial-state (parse-initial-state input)]
          (->> (reduce (fn [state _iteration]
                         (-> state
                             (prepare-space)
                             (perform-rules rules)))
                       initial-state
                       (range 0 20))
               (filter (fn [[_ plant]] (= plant \#)))
               (map first)
               (reduce +)))))
