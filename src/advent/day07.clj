(ns advent.day07
  (:require
   [clojure.string :as str]))

(defn parse-input [str]
  (->> str
       (str/split-lines)
       (map #(str/split % #":"))
       (map (fn [[target rest]]
              {:target (Long/parseLong target)
               :vals (->> (str/split rest #"\s+")
                          (filter seq)
                          (mapv #(Long/parseLong %)))}))))

(defn combine-with [ops [head & tail]]
  (reduce (fn [vs v] (filter #(not (nil? %)) (for [w vs f ops]  (f w v)))) (list head) tail))

(defn lcat [a b] (Long/parseLong (str/join (list a b))))

(defn luncat [a b] (let [[h t] (split-at (count (str b)) (str a))]
                     (if (and (> (count t) 0) (= h (seq (str b)))) (Long/parseLong (str/join t)) nil)))

(defn un-* [a b] (if (= 0 (rem a b)) (quot a b) nil))

(comment
  (= (first (split-at 3 "ae")) (seq "adc"))
  (split-at 3 "ae")
  (str/join (first (split-at 2 "abc")))
  (count "abc"))

(defn reachable [target vals ops]
  ((set (combine-with ops vals)) target))

(defn mitm [target vals ops inv-ops]
  (let [len (count vals)
        mid (quot len 2)
        [front back] (split-at mid vals)
        front-reach (combine-with ops front)
        back-reach (combine-with inv-ops (cons target (reverse back)))]
    (some #((set back-reach) %) front-reach)))

(defn part-1 [input]
  (->> input
       (map #(reachable (:target %) (:vals %) (list + *)))
       (filter #(not (nil? %)))
       (reduce +)))

(defn part-1-mitm [input]
  (->> input
       (filter #(mitm (:target %) (:vals %) (list + *) (list - un-*)))
       (map :target)
       (reduce +)))

(defn part-2 [input]
  (->> input
       (map #(reachable (:target %) (:vals %) (list + * lcat)))
       (filter #(not (nil? %)))
       (reduce +)))

(defn part-2-mitm [input]
  (->> input
       (filter #(mitm (:target %) (:vals %) (list + * lcat) (list - un-* luncat)))
       (map :target)
       (reduce +)))

(comment
  (luncat "12334" "123"))

(defn -main [& args]
  (let [input (parse-input (slurp (first args)))]
    (println "Part 1" (part-1 input))
    (println "Part 2" (part-2 input))))
