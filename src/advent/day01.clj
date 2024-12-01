(ns advent.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-columns [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (doall (line-seq rdr))
          columns (map #(str/split % #"   ") lines)
          col1 (map #(Integer/parseInt (first %)) columns)
          col2 (map #(Integer/parseInt (second %)) columns)]
      {:col1 (vec col1) :col2 (vec col2)})))
(defn pairwise-difference-sum [v1 v2]
  (->> (map vector v1 v2)
       (map (partial apply -))
       (map Math/abs)
       (reduce +)))

(defn map-values [f m]
  (->> m
       (map #(vector (first %) (f (second %))))
       (into {})))

(defn my-frequnecies [v]
  (->> v
       (group-by identity)
       (map-values count)))

(defn similarity-score [v1 v2]
  (let [counts (my-frequnecies v2)]
    (->> v1
         (map #(* % (get counts % 0)))
         (reduce +))))

(defn -main [& args]
  (let [file-path (first args)
        {:keys [col1 col2]} (read-columns file-path)]
    (println "Sum of pairwise differences:" (pairwise-difference-sum (sort col1) (sort col2)))
    (println "Similarity score:" (similarity-score col1 col2))))
