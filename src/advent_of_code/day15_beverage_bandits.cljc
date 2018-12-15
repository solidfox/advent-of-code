(ns advent-of-code.day15-beverage-bandits
  (:require [clojure.string :as str]
            [clojure.set :refer [difference]]
            [clojure.test :refer [is]]))

(def dungeon (vec (str/split-lines "################################\n#################.....##########\n#################..#.###########\n#################.........######\n##################......########\n#################G.GG###########\n###############...#..###########\n###############......G..########\n############..G.........########\n##########.G.....G......########\n##########......#.........#..###\n##########...................###\n#########G..G.#####....E.G.E..##\n######..G....#######...........#\n#######.....#########.........##\n#######..#..#########.....#.####\n##########..#########..G.##..###\n###########G#########...E...E.##\n#########.G.#########..........#\n#########GG..#######.......##.E#\n######.G......#####...##########\n#...##..G..............#########\n#...#...........###..E.#########\n#.G.............###...##########\n#................###############\n##.........E.....###############\n###.#..............#############\n###..G........E.....############\n###......E..........############\n###......#....#E#...############\n###....####.#...##.#############\n################################")))

(defn spy [x] (clojure.pprint/pprint x) x)

(defn is-actor? [char]
  (or (= char \G)
      (= char \E)))

(defn create-state
  {:test (fn [] (create-state dungeon))}
  [dungeon]
  (->> dungeon
       (map-indexed (fn [y row]
                      (->> row
                           (map-indexed vector)
                           (reduce (fn [[clean-row actors] [x char]]
                                     [(conj clean-row (if (is-actor? char) \. char))
                                      (if (is-actor? char)
                                        (conj actors {:origin   [x y]
                                                      :position [x y]
                                                      :team     char
                                                      :hp       200})
                                        actors)])
                                   [[] []]))))
       ((fn [rows-with-actors]
          {:dungeon (vec (map (fn [[row _]] (apply str row))
                              rows-with-actors))
           :actors  (vec (apply concat
                                (map second rows-with-actors)))}))))

(defn get-coordinate [dungeon [x y]]
  (if (or (< x 0) (< y 0)
          (<= (count (first dungeon)) x) (<= (count dungeon) y))
    \#
    (nth (nth dungeon y) x)))

(defn traversable? [state target-team coordinate]
  (and (= \. (get-coordinate (:dungeon state)
                             coordinate))
       (not (some (fn [{team     :team
                        position :position}]
                    (and (not= team target-team)
                         (= position coordinate)))
                  (:actors state)))))

(defn get-adjacent-coordinates [state {[x y]                 :coordinate
                                       processed-coordinates :processed-coordinates
                                       target-team           :target-team}]
  (as-> [[x (dec y)]
         [(dec x) y]
         [(inc x) y]
         [x (inc y)]] $
        (filter (fn [coordinates] (traversable? state target-team coordinates))
                $)
        (into #{} $)
        (difference $ processed-coordinates)))

(defn resolve-path-from-back-pointers [back-pointers position]
  (loop [path (list position)
         current-path-coord position]
    (let [previous-step (get back-pointers current-path-coord)]
      (if (nil? previous-step)
        path
        (recur (conj path previous-step)
               previous-step)))))

(def test-state (create-state ["#######"
                               "#.E...#"
                               "#.....#"
                               "#...G.#"
                               "#######"]))

(defn reading-order [coordinate-or-actor]
  (str/join "," (reverse (get coordinate-or-actor :position coordinate-or-actor))))


(defn find-nearest-target
  {:test (fn []
           (is (= (second (:path (find-nearest-target test-state
                                                      (first (:actors test-state)))))
                  [3 1])))}
  [state actor]
  (let [target-team (if (= \G (:team actor)) \E \G)
        candidate-targets (sort-by reading-order
                                   (->> (:actors state)
                                        (filter (fn [actor] (= (:team actor) target-team)))))]
    (when (not (empty? candidate-targets))
      (loop [processed-coords #{(:position actor)}
             fringe-coordinates #{(:position actor)}
             back-pointers {}]
        (let [[new-processed-coords new-back-pointers new-fringe]
              (reduce (fn [[processed-coords back-pointers new-fringe] fringe-coord]
                        (let [adjacents (get-adjacent-coordinates state {:coordinate            fringe-coord
                                                                         :processed-coordinates processed-coords
                                                                         :target-team           target-team})]
                          [(reduce conj processed-coords adjacents)
                           (reduce (fn [back-pointers adjacent] (assoc back-pointers adjacent fringe-coord))
                                   back-pointers
                                   adjacents)
                           (reduce conj new-fringe adjacents)]))
                      [processed-coords back-pointers #{}]
                      (sort-by reading-order
                               fringe-coordinates))
              targets-in-fringe (->> candidate-targets
                                     (filter (fn [candidate-target]
                                               (contains? new-fringe (:position candidate-target)))))]
          (if (not (empty? targets-in-fringe))
            (->> targets-in-fringe
                 (sort-by reading-order)
                 (first)
                 ((fn [target]
                    {:target target
                     :path   (resolve-path-from-back-pointers new-back-pointers (:position target))})))
            (recur new-processed-coords
                   new-fringe
                   new-back-pointers)))))))




(defn part1
  [dungeon]
  (let [state (create-state dungeon)]))
;(get-adjacent-coordinates state (spy (-> state :actors first :position)))))
