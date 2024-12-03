(ns advent.day03
  (:require [clojure.java.io :as io]))

(defn process-line [line]
  (->> line
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (partial drop 1))
       (map (partial map Integer/parseInt))
       (map (partial apply *))
       (reduce +)))

(defmulti token (fn [match]
                  (cond
                    (re-matches #"do\(\)" (match 0)) :do
                    (re-matches #"don't\(\)" (match 0)) :dont
                    :else :mul)))

(defmethod token :do [_] {:type :do :args []})

(defmethod token :dont [_] {:type :dont :args []})

(defmethod token :mul [match] {:type :mul :args [(match 2) (match 3)]})

(defmulti value (partial :type))

(defmethod value :do [_] 0)

(defmethod value :dont [_] 0)

(defmethod value :mul [match] (->> (:args match) (map Integer/parseInt) (reduce *)))

(defn tokenize [line]
  (->> line
       (re-seq #"(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))")
       (map token)))

(defn evaluate [tokens]
  (->>
   tokens
   (reduce (fn [{:keys [active accum]} token]
             (cond
               (= (:type token) :do) {:active true :accum accum}
               (= (:type token) :dont) {:active false :accum accum}
               active {:active active :accum (+ accum (value token))}
               :else {:active active :accum accum}))
           {:active true :accum 0})
   (:accum)))

(defn read-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (line-seq rdr)]
      (->>
       lines
       (map process-line)
       (reduce +)))))

(defn read-file-with-state [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (line-seq rdr)]
      (->>
       lines
       (mapcat tokenize)
       (evaluate)))))

(defn -main [& args]
  (let [file-path (first args)]
    (println "Sum of all muls:" (read-file file-path))
    (println "Sum of all muls with state:" (read-file-with-state file-path))))
