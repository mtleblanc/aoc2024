(ns advent.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (line-seq rdr)]
      (->>
       lines
       (map #(str/split % #"\s+"))
       (map (partial map Integer/parseInt))
       (vec)))))

(defn safe? [coll]
  (let [deltas (->> coll (partition 2 1) (map (partial apply -)))]
    (or
     (every? #(and (>= % 1) (<= % 3)) deltas)
     (every? #(and (<= % -1) (>= % -3)) deltas))))

(defn count-safe [coll]
  (->> coll
       (filter safe?)
       (count)))

(defn choose-n-minus-1 [coll]
  (map-indexed (fn [i _] (concat (take i coll) (drop (inc i) coll))) coll))

(defn relaxed-safe? [coll]
  (or (safe? coll)
      (->> coll
           choose-n-minus-1
           (some safe?))))

(defn count-relaxed-safe [coll]
  (->>
   coll
   (filter relaxed-safe?)
   (count)))


(defn -main [& args]
  (let [file-path (first args)
        readings (read-file file-path)]
    (println "Number of safe readings:" (count-safe readings))
    (println "Number of relaxed safe readings:" (count-relaxed-safe readings))))
