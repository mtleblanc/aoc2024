(ns advent.factorial
  (:require [clojure.java.io :as io]))

(defn factorial [n]
  (apply * (range 1 (inc n))))

(defn process-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (doseq [line (line-seq rdr)]
      (let [n (Integer/parseInt line)]
        (println (str "Factorial of " n ": " (factorial n)))))))

(defn -main [{:keys [file-path]}]
  (process-file file-path))
