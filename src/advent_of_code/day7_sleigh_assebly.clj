(ns advent-of-code.day7-sleigh-assebly
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.set :refer [intersection union difference]]))

(def input "Step P must be finished before step R can begin.\nStep V must be finished before step J can begin.\nStep O must be finished before step K can begin.\nStep S must be finished before step W can begin.\nStep H must be finished before step E can begin.\nStep K must be finished before step Y can begin.\nStep B must be finished before step Z can begin.\nStep N must be finished before step G can begin.\nStep W must be finished before step I can begin.\nStep L must be finished before step Y can begin.\nStep U must be finished before step Q can begin.\nStep R must be finished before step Z can begin.\nStep Z must be finished before step E can begin.\nStep C must be finished before step I can begin.\nStep I must be finished before step Q can begin.\nStep D must be finished before step E can begin.\nStep A must be finished before step J can begin.\nStep G must be finished before step Y can begin.\nStep M must be finished before step T can begin.\nStep E must be finished before step X can begin.\nStep F must be finished before step T can begin.\nStep X must be finished before step J can begin.\nStep Y must be finished before step J can begin.\nStep T must be finished before step Q can begin.\nStep J must be finished before step Q can begin.\nStep E must be finished before step Y can begin.\nStep A must be finished before step T can begin.\nStep P must be finished before step H can begin.\nStep W must be finished before step R can begin.\nStep Y must be finished before step Q can begin.\nStep W must be finished before step M can begin.\nStep O must be finished before step M can begin.\nStep H must be finished before step R can begin.\nStep N must be finished before step L can begin.\nStep V must be finished before step W can begin.\nStep S must be finished before step Q can begin.\nStep D must be finished before step J can begin.\nStep W must be finished before step E can begin.\nStep V must be finished before step Y can begin.\nStep O must be finished before step C can begin.\nStep B must be finished before step T can begin.\nStep W must be finished before step T can begin.\nStep G must be finished before step T can begin.\nStep D must be finished before step T can begin.\nStep P must be finished before step E can begin.\nStep P must be finished before step J can begin.\nStep G must be finished before step E can begin.\nStep Z must be finished before step M can begin.\nStep K must be finished before step T can begin.\nStep H must be finished before step U can begin.\nStep P must be finished before step T can begin.\nStep W must be finished before step A can begin.\nStep A must be finished before step F can begin.\nStep F must be finished before step Y can begin.\nStep H must be finished before step M can begin.\nStep T must be finished before step J can begin.\nStep O must be finished before step S can begin.\nStep P must be finished before step M can begin.\nStep X must be finished before step T can begin.\nStep S must be finished before step J can begin.\nStep H must be finished before step C can begin.\nStep B must be finished before step W can begin.\nStep K must be finished before step N can begin.\nStep E must be finished before step T can begin.\nStep S must be finished before step Y can begin.\nStep C must be finished before step G can begin.\nStep R must be finished before step D can begin.\nStep N must be finished before step U can begin.\nStep O must be finished before step L can begin.\nStep B must be finished before step F can begin.\nStep S must be finished before step F can begin.\nStep X must be finished before step Y can begin.\nStep S must be finished before step D can begin.\nStep R must be finished before step E can begin.\nStep S must be finished before step A can begin.\nStep S must be finished before step X can begin.\nStep A must be finished before step G can begin.\nStep E must be finished before step F can begin.\nStep P must be finished before step A can begin.\nStep A must be finished before step M can begin.\nStep E must be finished before step Q can begin.\nStep H must be finished before step W can begin.\nStep W must be finished before step U can begin.\nStep F must be finished before step Q can begin.\nStep I must be finished before step J can begin.\nStep H must be finished before step G can begin.\nStep I must be finished before step G can begin.\nStep P must be finished before step X can begin.\nStep I must be finished before step D can begin.\nStep R must be finished before step X can begin.\nStep S must be finished before step I can begin.\nStep Y must be finished before step T can begin.\nStep R must be finished before step G can begin.\nStep I must be finished before step X can begin.\nStep B must be finished before step D can begin.\nStep X must be finished before step Q can begin.\nStep F must be finished before step X can begin.\nStep V must be finished before step R can begin.\nStep C must be finished before step J can begin.\nStep L must be finished before step Q can begin.\nStep K must be finished before step B can begin.")

(defn extract-dependencies [input]
  (->> input
       (str/split-lines)
       (map (comp (partial drop 1)
                  (partial re-matches #"Step (.) must be finished before step (.) can begin.")))))

(defn get-map-of-dependencies [dependency-list]
  (let [empty-map (as-> dependency-list $
                        (flatten $)
                        (into #{} $)
                        (map vector $ (repeat #{}))
                        (into {} $))]
    empty-map
    (reduce (fn [dep-map [from-step to-step]]
              (update dep-map to-step conj from-step))
            empty-map
            dependency-list)))

(defn spy [x] (clojure.pprint/pprint x) x)

(defn next-step
  {:test (fn []
           (is (= (next-step {:done           []
                              :dependency-map {"A" #{"B" "C" "D"}
                                               "B" #{"C"}
                                               "C" #{}
                                               "D" #{}}}) "C"))
           (is (= (next-step {:done           ["C" "B" "D" "A"]
                              :dependency-map {"A" #{"B" "C" "D"}
                                               "B" #{"C"}
                                               "C" #{}
                                               "D" #{}}}) nil))
           (is (= (next-step {:started        #{"C" "B" "D" "A"}
                              :dependency-map {"A" #{"B" "C" "D"}
                                               "B" #{"C"}
                                               "C" #{}
                                               "D" #{}}}) nil)))}
  [{:keys [started done dependency-map]
    :or   {started #{}}}]
  (let [done (into #{} done)]
    (->> (keys dependency-map)
         (filter (fn [step]
                   (let [dependencies (get dependency-map step)]
                     (and (not (contains? started step))
                          (not (contains? done step))
                          (= (clojure.set/intersection done dependencies)
                             dependencies)))))
         (sort)
         (first))))

(defn get-sequence [dependency-map]
  (loop [sequence []]
    (if-let [next-step (next-step {:done           sequence
                                   :dependency-map dependency-map})]
      (recur (concat sequence [next-step]))
      (str/join sequence))))

(defn part1 [input]
  (->> input
       extract-dependencies
       get-map-of-dependencies
       get-sequence))

(def time-for-step
  (as-> (range (int \A) (+ 1 (int \Z))) $
        (map (fn [step-char-int time] [(str (char step-char-int)) (+ time 60)])
             $ (range 1 (+ 1 (count $))))
        (into {} $)))

(defn get-available-worker [worker-map]
  (->> worker-map
       (filter (fn [[worker {doing :doing}]]
                 (nil? doing)))
       (ffirst)))

(defn free-worker
  {:test (fn []
           (is (= (free-worker {:a {:doing     "A"
                                    :time-left 30}
                                :b {:doing     "B"
                                    :time-left 60}})
                  [30 #{"A"} {:a {:doing     nil
                                  :time-left 0}
                              :b {:doing     "B"
                                  :time-left 30}}]))
           (is (= (free-worker {:a {:doing     nil
                                    :time-left 0}
                                :b {:doing     "B"
                                    :time-left 60}})
                  [60 #{"B"} {:a {:doing     nil
                                  :time-left 0}
                              :b {:doing     nil
                                  :time-left 0}}]))
           (is (= (free-worker {:a {:doing "O", :time-left 75},
                                :b {:doing "P", :time-left 76},
                                :c {:doing "V", :time-left 82},
                                :d {:doing nil, :time-left 0},
                                :e {:doing nil, :time-left 0}})
                  [75 #{"O"} {:a {:doing nil, :time-left 0},
                              :b {:doing "P", :time-left 1},
                              :c {:doing "V", :time-left 7},
                              :d {:doing nil, :time-left 0},
                              :e {:doing nil, :time-left 0}}])))}
  [worker-map]
  (let [time-needed (->> worker-map
                         (map (fn [[_worker {time-left :time-left}]]
                                time-left))
                         (remove (fn [time-left] (= time-left 0)))
                         (apply min))]
    (->> worker-map
         (map (fn [[worker {:keys [doing time-left]}]]
                [worker {:doing     doing
                         :time-left (max 0 (- time-left time-needed))}]))
         ((fn [new-worker-map]
            [time-needed
             (->> new-worker-map
                  (filter (fn [[_ {:keys [time-left]}]] (= time-left 0)))
                  (map (fn [[_ {:keys [doing]}]] doing))
                  (remove nil?)
                  (into #{}))
             (->> new-worker-map
                  (map (fn [[worker {:keys [time-left doing]}]]
                         [worker {:doing     (when (not= 0 time-left) doing)
                                  :time-left time-left}]))
                  (into {}))])))))

(defn all-workers-finished?
  {:test (fn []
           (is (all-workers-finished? (->> [:a :b :c :d :e]
                                           (map (fn [worker] [worker {:doing     nil
                                                                      :time-left 0}]))
                                           (into {}))))
           (is (not (all-workers-finished? (->> [:a :b :c :d :e]
                                                (map (fn [worker] [worker {:doing     "A"
                                                                           :time-left 30}]))
                                                (into {}))))))}
  [worker-map]
  (every? (fn [[_ {doing :doing}]]
            (nil? doing))
          worker-map))

(defn get-time-to-assemble
  [time-for-step worker-map dependency-map]
  (loop [started           #{}
         finished          #{}
         worker-map        worker-map
         time-spent-so-far 0]
    (if (and (= (into #{} started) (into #{} (keys dependency-map)))
             (all-workers-finished? worker-map))
      time-spent-so-far
      (let [available-worker (get-available-worker worker-map)
            step-to-start    (next-step {:started        started
                                         :done           finished
                                         :dependency-map dependency-map})]
        (if (and available-worker
                 step-to-start)
          (recur (conj started step-to-start)
                 finished
                 (assoc worker-map available-worker {:doing     step-to-start
                                                     :time-left (get time-for-step step-to-start)})
                 time-spent-so-far)
          (let [[time-spent done-tasks new-worker-map] (free-worker worker-map)]
            (recur started
                   (union finished done-tasks)
                   new-worker-map
                   (+ time-spent time-spent-so-far))))))))

(defn part2 [input]
  (let [workers (->> [:a :b :c :d :e]
                     (map (fn [worker] [worker {:doing     nil
                                                :time-left 0}]))
                     (into {}))]
    (->> input
         extract-dependencies
         get-map-of-dependencies
         (get-time-to-assemble time-for-step workers))))












