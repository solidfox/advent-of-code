(ns advent-of-code.day15-beverage-bandits
  (:require [clojure.string :as str]
            [clojure.set :refer [difference]]
            [clojure.test :refer [is]]
            [ysera.test :refer [is= is-not]]))

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

(defn other-team
  [team]
  (when team
    (if (= \G team) \E \G)))

(def test-state (create-state ["#######"
                               "#.E...#"
                               "#.....#"
                               "#...G.#"
                               "#######"]))

(def test-state-2 (create-state ["#######"
                                 "#.G...#"
                                 "#...EG#"
                                 "#.#.#G#"
                                 "#..G#E#"
                                 "#.....#"
                                 "#######"]))

(def test-state-3 (create-state ["#######"
                                 "#..G..#"
                                 "#...EG#"
                                 "#.#G#G#"
                                 "#...#E#"
                                 "#.....#"
                                 "#######"]))

(defn get-adjacent-coordinates
  {:test (fn [] (is (= (get-adjacent-coordinates test-state-2 {:coordinate  [4 1]
                                                               :target-team \E})
                       #{[5 1] [4 2] [3 1]})))}
  [state {actor                 :actor
          coordinate            :coordinate
          processed-coordinates :processed-coordinates
          target-team           :target-team
          :or                   {processed-coordinates #{}}}]
  (let [[x y] (or (:position actor) coordinate)
        target-team (or (other-team (:team actor)) target-team)]
    (as-> [[x (dec y)]
           [(dec x) y]
           [(inc x) y]
           [x (inc y)]] $
          (filter (fn [coordinates] (traversable? state target-team coordinates))
                  $)
          (into #{} $)
          (difference $ processed-coordinates))))

(defn resolve-path-from-back-pointers [back-pointers position]
  (loop [path (list position)
         current-path-coord position]
    (let [previous-step (get back-pointers current-path-coord)]
      (if (nil? previous-step)
        path
        (recur (conj path previous-step)
               previous-step)))))


(defn reading-order [coordinate-or-actor]
  (str/join "," (reverse (get coordinate-or-actor :position coordinate-or-actor))))

(defn get-actor [state actor-id]
  (reduce (fn [_ actor]
            (when (= (:origin actor) actor-id)
              (reduced actor)))
          nil
          (:actors state)))

(defn find-nearest-target
  {:test (fn []
           (is (= (second (:path (find-nearest-target test-state
                                                      [2 1])))
                  [3 1]))
           (is (= (second (:path (find-nearest-target test-state-2
                                                      [2 1])))
                  [3 1])))}
  [state actor-id]
  (let [actor (get-actor state actor-id)
        target-team (other-team (:team actor))
        candidate-targets (->> (:actors state)
                               (filter (fn [actor] (= (:team actor) target-team))))]
    (when (not (empty? candidate-targets))
      (loop [processed-coords #{(:position actor)}
             fringe-coordinates #{(:position actor)}
             back-pointers {}]
        (when (not-empty fringe-coordinates)
          (let [[new-processed-coords new-fringe new-back-pointers]
                (reduce (fn [[processed-coords new-fringe back-pointers] fringe-coord]
                          (let [adjacents (get-adjacent-coordinates state {:coordinate            fringe-coord
                                                                           :processed-coordinates processed-coords
                                                                           :target-team           target-team})]
                            [(reduce conj processed-coords adjacents)
                             (reduce conj new-fringe adjacents)
                             (reduce (fn [back-pointers adjacent] (assoc back-pointers adjacent fringe-coord))
                                     back-pointers
                                     adjacents)]))
                        [processed-coords #{} back-pointers]
                        (sort-by reading-order
                                 fringe-coordinates))
                targets-in-fringe
                (->> candidate-targets
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
                     new-back-pointers))))))))

(defn move-actor
  [state actor-id-to-move new-position]
  (update state :actors
          (fn [actors]
            (map (fn [actor]
                   (if (= (:origin actor) actor-id-to-move)
                     (assoc actor :position new-position)
                     actor))
                 actors))))

(defn get-actor-at-position [state coordinate]
  (->> state
       :actors
       (filter (comp (partial = coordinate) :position))
       (first)))

(defn alive? [actor]
  (>= (:hp actor) 0))

(defn attack-actor [state actor-id-to-attack]
  (update state :actors
          (fn [actors]
            (->> actors
                 (map (fn [actor]
                        (if (= (:origin actor) actor-id-to-attack)
                          (update actor :hp - 3)
                          actor)))
                 (filter alive?)))))

(defn min-hp [actor-a actor-b]
  (if (< (get actor-b :hp 9999) (get actor-a :hp 9999))
    actor-b
    actor-a))

(def attackable-state (create-state ["#######"
                                     "#.EG..#"
                                     "#.G...#"
                                     "#.....#"
                                     "#######"]))

(defn find-attackable-target
  {:test (fn []
           (is (= (:position (find-attackable-target (attack-actor attackable-state
                                                                   (nth (:actors attackable-state) 2))
                                                     (:origin (first (:actors attackable-state)))))
                  [2 2])))}
  [state attacking-actor-id]
  (let [attacking-actor (get-actor state attacking-actor-id)]
    (as-> attacking-actor $
          (get-adjacent-coordinates state {:actor $})
          (reduce (fn [actor-to-attack coordinate]
                    (let [candidate-actor (get-actor-at-position state coordinate)]
                      (if (= (:team candidate-actor)
                             (other-team (:team attacking-actor)))
                        (min-hp actor-to-attack candidate-actor)
                        actor-to-attack)))
                  nil
                  $))))

(defn n-teams [state]
  (->> state
       :actors
       (map :team)
       (into #{})
       (count)))

(defn enact-turn
  {:test (fn []
           (is (= (-> (enact-turn test-state-2
                                  [2 1])
                      (get-actor [2 1])
                      :position)
                  [3 1]))
           (is (= (-> (enact-turn test-state-3
                                  [3 1])
                      (get-actor [4 2])
                      :hp)
                  197)))}
  [state actor-id]
  (if (= 1 (n-teams state))
    (reduced (assoc state :end-game true))
    (if (not (get-actor state actor-id))
      state
      (if-let [initially-attackable-target (find-attackable-target state actor-id)]
        (attack-actor state (:origin initially-attackable-target))
        (if-let [{path-to-nearest-target :path} (find-nearest-target state actor-id)]
          (as-> state $state
                (move-actor $state actor-id (second path-to-nearest-target))
                (if-let [attackable-target (find-attackable-target $state actor-id)]
                  (attack-actor $state (:origin attackable-target))
                  $state))
          state)))))

(defn enact-round
  {:test (fn [] (as-> (enact-round test-state-3) $
                      (do
                        (is= (get-actor $ [3 1])
                             {:origin [3 1], :position [4 1], :team \G, :hp 197})
                        (is= (get-actor $ [4 2])
                             {:origin [4 2], :position [4 2], :team \E, :hp 191}))))}
  [state]
  (let [actor-id-order (->> state
                            :actors
                            (sort-by reading-order)
                            (map :origin))]
    (reduce enact-turn state actor-id-order)))

(defn visualize-state [state]
  (str/join "\n"
            (map-indexed (fn [y row]
                           (str/join (map-indexed (fn [x cave-char]
                                                    (or (:team (get-actor-at-position state [x y]) cave-char)))
                                                  row)))
                         (:dungeon state))))

(defn part1
  {:test (fn []
           (is= (part1 ["#######"
                        "#G..#E#"
                        "#E#E.E#"
                        "#G.##.#"
                        "#...#E#"
                        "#...E.#"
                        "#######"])
                36334)
           (is= (part1 ["#######"
                        "#E..EG#"
                        "#.#G.E#"
                        "#E.##E#"
                        "#G..#.#"
                        "#..E#.#"
                        "#######"])
                39514)
           (is= (part1 ["#######"
                        "#.G...#"
                        "#...EG#"
                        "#.#.#G#"
                        "#..G#E#"
                        "#.....#"
                        "#######"])
                27730)
           (is= (part1 ["#######"
                        "#E.G#.#"
                        "#.#G..#"
                        "#G.#.G#"
                        "#G..#.#"
                        "#...E.#"
                        "#######"])
                27755)
           (is= (part1 ["#######"
                        "#.E...#"
                        "#.#..G#"
                        "#.###.#"
                        "#E#G#G#"
                        "#...#G#"
                        "#######"])
                28944)
           (is= (part1 ["#########"
                        "#G......#"
                        "#.E.#...#"
                        "#..##..G#"
                        "#...##..#"
                        "#...#...#"
                        "#.G...G.#"
                        "#.....G.#"
                        "#########"])
                18740))}

  [dungeon]
  (let [state (create-state dungeon)]
    (loop [state state
           rounds 0]
      (println rounds)
      (clojure.pprint/pprint (sort-by reading-order (:actors state)))
      (println (visualize-state state))
      (let [next-state (enact-round state)]
        (if (:end-game next-state)
          (do
            (clojure.pprint/pprint (sort-by reading-order (:actors next-state)))
            (println (visualize-state next-state))
            (* rounds (->> next-state
                           :actors
                           (map :hp)
                           (reduce +))))
          (recur next-state
                 (inc rounds)))))))
