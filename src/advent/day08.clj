(ns advent.day08
  (:require
   [clojure.string :as str]))

(defn to-sparse [coll]
  (let [with-indices (for [x (range (count coll))
                           y (range (count (coll x)))]
                       [x y (get-in coll [x y])])]
    (reduce (fn [acc [x y v]]
              (if (= v \.) acc
                  (update acc v #(into [[x y]] %))))
            {}
            with-indices)))

(defn parse-input [input]
  (->> input str/split-lines (mapv vec)))

(defn diff [v w] (mapv - v w))
(defn sum [v w] (mapv + v w))
(defn node [v w] (sum v (diff v w)))

(defn in-bounds [rows cols] (fn [[x y]]
                              (and (>= x 0) (< x rows) (>= y 0) (< y cols))))
(defn part-1 [input] (let [grid (parse-input input)]
                       (->>
                        grid
                        to-sparse
                        vals
                        (mapcat (fn [coords] (for [c coords d coords] (if (not= c d) (node c d)))))
                        (remove nil?)
                        (set)
                        (filter (in-bounds (count grid) (count (grid 0))))
                        (count))))

(defn -main [& args]
  (let [file-path (first args)
        input (slurp file-path)]
    (println "Part 1" (part-1 input))))
