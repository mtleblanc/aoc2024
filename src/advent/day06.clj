(ns advent.day06
  (:require
   [clojure.java.io :as io]))

(defn parse-line [line] (mapv (fn [c] (cond
                                        (= c \.) 0
                                        (= c \#) 1
                                        :else 2)) line))

(defn find-start [grid]
  (->> grid
       (map (fn [row] (keep-indexed #(when (= 2 %2) %1) row)))
       (keep-indexed #(when (seq %2) [%1 (first %2)]))
       first))

(defn in-grid [grid [x y]]
  (let [x-size (count (first grid))
        y-size (count grid)]
    (and (< x x-size) (< y y-size) (>= x 0) (>= y 0))))

(defn can-move [grid [x y] [dx dy]]
  (let [x2 (+ x dx)
        y2 (+ y dy)]
    (or (not (in-grid grid [x2 y2]))
        (not= 1 (get-in grid [x2 y2])))))

(defn run [grid]
  (let [start (find-start grid)
        facing-delta [[-1 0] [0 1] [1 0] [0 -1]]]
    (if (nil? start) 0
        (loop [seen {start #{0}}
               location start
               facing 0]
          (let [delta (facing-delta facing)
                next (mapv + location delta)]
            (cond
              (contains? (get seen next) facing) nil
              (not (in-grid grid location)) (keys seen)
              (can-move grid location (facing-delta facing)) (recur (update seen next (partial into #{facing})) next facing)
              :else (recur seen location (mod (inc facing) 4))))))))

(defn obstacles [grid checks]
  (->> checks
       (filter #(= 0 (get-in grid %)))
       (filter (fn [location]
                 (let [test-grid (assoc-in grid location 1)]
                   (nil? (run test-grid)))))
       count))

(defn read-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (line-seq rdr)
          grid
          (->>
           lines
           (mapv parse-line))]
      grid)))

(defn -main [& args]
  (let [file-path (first args)
        grid (read-file file-path)
        visited (run grid)]
    (println "Locations searched" (dec (count visited)))
    (println "Locations for obstacles" (obstacles grid visited))))
