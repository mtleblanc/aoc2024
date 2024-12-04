(ns advent.day04
  (:require [clojure.java.io :as io]))


(defn read-file-as-2d-vec [file-path]
  (with-open [rdr (io/reader file-path)]
    (->> rdr line-seq (mapv vec))))

(defn match-at [needle array x y dx dy]
  (cond
    (empty? needle) true
    (< x 0) false
    (< y 0) false
    (>= x (count array)) false
    (>= y (count (first array))) false
    (not= (get-in array [x y]) (first needle)) false
    :else (match-at (rest needle) array (+ x dx) (+ y dy) dx dy)))

(defn x-mas-at [array x y]
  (cond
    (< x 1) false
    (< y 1) false
    (>= (inc x) (count array)) false
    (>= (inc y) (count (first array))) false
    (not= (get-in array [x y]) \A) false
    :else (let [d1 (set [(get-in array [(inc x) (inc y)]) (get-in array [(dec x) (dec y)])])
                d2 (set [(get-in array [(inc x) (dec y)]) (get-in array [(dec x) (inc y)])])]
            (and
             (contains? d1 \M)
             (contains? d1 \S)
             (contains? d2 \M)
             (contains? d2 \S)))))

(defn run-xmas [word word-search]
  (->>
   (for [x (range 0 (count word-search)) y (range 0 (count (first word-search))) dx (range -1 2) dy (range -1 2)]
     (match-at word word-search x y dx dy))
   (filter identity)
   (count)))

(defn run-x-mas [word-search]
  (->>
   (for [x (range 0 (count word-search)) y (range 0 (count (first word-search)))]
     (x-mas-at word-search x y))
   (filter identity)
   (count)))

(defn -main [& args]
  (let [file-path (first args)
        word-search (read-file-as-2d-vec file-path)]
    (println "XMAS:" (run-xmas "XMAS" word-search))
    (println "X-MAS:" (run-x-mas word-search))))
