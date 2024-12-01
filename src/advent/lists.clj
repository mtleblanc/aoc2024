(ns advent.lists
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-columns [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (doall (line-seq rdr)) ; Realize the lazy sequence
          columns (map #(str/split % #"   ") lines)
          col1 (map #(Integer/parseInt (first %)) columns)
          col2 (map #(Integer/parseInt (second %)) columns)]
      {:col1 col1 :col2 col2}))) ; Use vectors to realize lists

(defn -main [& args]
  (let [file-path (first args)
        {:keys [col1 col2]} (read-columns file-path)]
    (println "Column 1:" col1)
    (println "Column 2:" col2)))
