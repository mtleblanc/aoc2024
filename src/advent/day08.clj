(ns advent.day08
  (:require
   [clojure.set :as set]
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
(defn gcd [a b] (cond (< b 0) (gcd a (- b)) (> b a) (gcd b a) (= b 0) a :else (gcd b (rem a b))))
(defn in-bounds [rows cols]
  (fn [[x y]]
    (and (>= x 0) (< x rows)
         (>= y 0) (< y cols))))

(defn all-nodes [rows cols v w]
  (if (= v w) #{}
      (let [d-orig (diff v w)
            g (apply gcd d-orig)
            d (mapv #(/ % g) d-orig)
            v? (in-bounds rows cols)]
        (->> v (iterate #(sum d %))
             (take-while v?)
             set))))


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

(defn part-2 [input] (let [grid (parse-input input)
                           rows (count grid)
                           cols (count (grid 0))]
                       (->> grid
                            to-sparse
                            vals
                            (mapcat (fn [coords] (for [c coords d coords] (all-nodes rows cols c d))))
                            (reduce set/union)
                            count)))

(defn -main [& args]
  (let [file-path (first args)
        input (slurp file-path)]
    (println "Part 1" (part-1 input))
    (println "Part 2" (part-2 input))))
