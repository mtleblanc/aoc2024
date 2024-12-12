(ns advent.day10
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-input [input] (->>
                           input
                           str/trim
                           str/split-lines
                           (mapv #(mapv Integer/parseInt (map str (seq %))))))


(def dirs [[0 1] [1 0] [0 -1] [-1 0]])

(defn bfs-step [grid] (fn [loc]
                        (let [current (get-in grid loc)
                              target (inc current)]
                          (vec (filter #(= target (get-in grid %)) (map #(mapv + loc %) dirs))))))


(defn explore-seen [grid] (comp (partial reduce set/union) (partial map (comp set (bfs-step grid)))))

(defn score [loc grid]
  (if (not= 0 (get-in grid loc)) 0
      (count (nth (iterate (explore-seen grid) #{loc}) 9))))


(defn part-1 [input] (let [grid (parse-input input)
                           rows (count grid)
                           cols (count (grid 0))]
                       (reduce + (for [r (range rows) c (range cols)] (score [r c] grid)))))

(defn bfs-with-count [grid] (fn [[loc count]] (zipmap ((bfs-step grid) loc) (repeat count))))

(defn sum-maps [maps]
  (reduce #(merge-with + %1 %2) maps))

(defn explore-with-count [grid] (fn [frontier]
                                  (sum-maps (mapv (bfs-with-count grid) frontier))))

(defn part-2 [input] (let [grid (parse-input input)
                           rows (count grid)
                           cols (count (grid 0))
                           locs (for [r (range rows) c (range cols)] [r c])
                           trailheads (filter #(= 0 (get-in grid %)) locs)
                           init (zipmap trailheads (repeat 1))]
                       (reduce + (map second (nth (iterate (explore-with-count grid) init) 9)))))

(defn -main [file-name] (let [input (slurp file-name)]
                          (println "Part 1" (part-1 input))
                          (println "Part 2" (part-2 input))))
