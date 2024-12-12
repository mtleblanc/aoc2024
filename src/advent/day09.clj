(ns advent.day09
  (:require
   [clojure.string :as str]) (:use
                              [clojure.data.finger-tree]))


(defn part-1' [input]
  (loop [l 0
         r (dec (count input))
         present true
         l-used 0
         l-val 0
         r-used 0
         r-val (quot (count input) 2)
         index 0
         accum 0]
    (cond
      (> l r) accum
      (and (= l r) (>= (+ l-used r-used) (input l))) accum
      (and present (< l-used (input l))) (recur l r present (inc l-used) l-val r-used r-val (inc index) (+ accum (* index l-val)))
      present (recur (inc l) r false 0 l-val r-used r-val index accum)
      (>= l-used (input l)) (recur (inc l) r true 0 (inc l-val) r-used r-val index accum)
      (< r-used (input r)) (recur l r present (inc l-used) l-val (inc r-used) r-val (inc index) (+ accum (* index r-val)))
      :else (recur l (- r 2) present l-used l-val 0 (dec r-val) index accum))))

(defn part-1 [input]
  (if (= 0 (mod (count input) 2)) (part-1 (subs input 0 (- (count input) 1)))
      (part-1' (mapv Long/parseLong (map str input)))))

(defn parse-input [input] (->>
                           (:list
                            (reduce (fn [{:keys [empty index id list]} val]
                                      (if empty
                                        {:empty false :index (+ index val) :id id :list (conj list {:id -1, :start index :length val})}
                                        {:empty true :index (+ index val) :id (inc id) :list (conj list {:id id, :start index, :length val})}))
                                    {:empty false :index 0 :id 0 :list []}
                                    (map #(Long/parseLong (str %)) (seq (str/trim input)))))
                          identity))
(comment 
  (type (parse-input "1234")))
(defn defrag [list elem] (do
                           ;(println "list" list "elem" elem)
                           (let [orig-id (:id elem)
                                 orig-length (:length elem)
                                 [head tail] (split-with (fn [{:keys [id length]}]
                                                           (or (and (>= id 0) (not= orig-id id))
                                                               (< length orig-length))) list)]
                            ;;  (println "Head" head) 
                            ;;  (println "Tail" tail)
                            ;;  (println "Orig-id" orig-id)
                            ;;  (println "Orig-lenth" orig-length)
                             (cond (empty? tail) list
                                   (= (:id (first tail)) orig-id) list
                                   :else (let [old-gap (first tail)
                                               new-gap (assoc old-gap :length (- (:length old-gap) orig-length) :start (+ (:start old-gap) orig-length))
                                               [tail-head tail-tail] (split-with #(not= orig-id (:id %)) (rest tail))]
                                           (vec (concat head [(assoc elem :start (:start old-gap)) new-gap] tail-head (rest tail-tail))))))))

(defn tri [n] (if (< n 0) 0 (/ (* n (inc n)) 2)))

(defn part-2 [input] (let [list (parse-input input)
                           elems (reverse (filter #(not= -1 (:id %)) list))]
                       (->> (reduce defrag list elems)
                            (map (fn [{:keys [length start id]}]
                                   (if (< id 1) 0
                                       (* id (- (tri (+ length (dec start))) (tri (dec start)))))))
                            (reduce +))))


(defn -main [& args] (let [input (slurp (first args))]
                       (println (part-1 input))
                       (println (part-2 input))))
