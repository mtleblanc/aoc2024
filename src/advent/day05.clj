(ns advent.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-dep [graph line]
  (let [[from to] (str/split line #"\|")]
    (merge-with into {from [to]} graph)))

;; (defn valid? [graph order]
;;   (loop [seen #{}
;;          frontier []
;;          order order]
;;     (println "Seen" seen "Frontier" frontier "Order" order)
;;     (cond (empty? order) true
;;           (empty? frontier)
;;           (if (contains? seen (first order)) false
;;               (recur (conj seen (first order)) (conj frontier (first order)) (rest order)))
;;           :else (let [[f & fs] frontier
;;                       es (get graph f [])
;;                       new-es (filter #(not (contains? seen %)) es)]
;;                  (recur
;;                  (into seen new-es)
;;                  (concat fs new-es)
;;                  order)))))

(defn valid? [graph order]
  (loop [order order seen #{}]
    (let [[head & rest] order
          deps (get graph head [])]
      (cond
        (empty? order) true
        (some #(contains? seen %) deps) false
        :else (recur rest (conj seen head))))))

(defn middle [xs]
  (let [l (count xs) m (/ (- l 1) 2) res (get xs m)]
    ;; (println xs l m res)
    res))

(defn compare-by-deps [graph a b]
  (let [a-deps (get graph a [])
        b-after (some #{b} a-deps)]
    ;; (println a a-deps b b-after)
    (if b-after -1 1)))

(defn sort-by-deps [graph order]
  (let [sorted (sort #(compare-by-deps graph %1 %2) order)]
    ;; (println order sorted)
    (vec sorted)))

(defn parse-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (line-seq rdr)
          [edges orders] (split-with seq lines)
          graph (reduce parse-dep {} edges)
          orders
          (->>
           orders
           (filter seq)
           (map #(str/split % #",")))
          part-1 (->>
                  orders
                  (filterv (partial valid? graph))
                  (map middle)
                  (map Integer/parseInt)
                  (reduce +))
          part-2 (->>
                  orders
                  (filterv #(not (valid? graph %)))
                  (map #(sort-by-deps graph %))
                  (map middle)
                  (map Integer/parseInt)
                  (reduce +))]

      [part-1 part-2])))

(defn -main [& args]
  (let [file-path (first args)
        result (parse-file file-path)]
    (println result)))
