(ns n-puzzle.solver
  (:require [clojure.data.priority-map :as p]
            [n-puzzle.generator :refer :all]))

(defn solvable?
  "Determine puzzle solvability"
  [puzzle]
  (let [board (:puzzle puzzle)
        tiles (remove zero? board)
        yblank (quot (.indexOf board 0) (:size puzzle))
        invertions (reduce +
                           (for [tile tiles
                                 :let [following (drop-while #(not= tile %) tiles)
                                       permutable (filter #(> tile %) following)]]
                             (count permutable)))]
    (cond
      (and (odd? (:size puzzle)) (odd? invertions))
      true
      (and (even? (:size puzzle))
           (or (and (even? yblank) (odd? invertions))
               (and (odd? yblank) (even? invertions))))
      true
      :else false)))

(defn conflicts
  "Returns conflict between 2 vectors"
  [p q]
  (keys (filter
          (fn [[k v]] (and (= v 2)
                           (not= (.indexOf p k) (.indexOf q k))))
          (frequencies (concat p q)))))

;; may still be buggy
(defn linear-conflict
  "Returns a conflict value of a puzzle"
  [puzzle]
  (let [size (:size puzzle)
        rows (map vec (partition size (:puzzle puzzle)))
        cols (apply map vector rows)
        goal (solved size)
        grows (map vec (partition size goal))
        gcols (apply map vector grows)]
    (count (flatten (map (fn [n]
                          (concat
                           (conflicts (nth rows n) (nth grows n))
                           (conflicts (nth cols n) (nth gcols n))))
                        (range size))))))

(defn board-heuristic
  "Generates heuristic value by comparing every case coord with solved puzzle with h
  then reducing it"
  [puzzle h]
  (let [size (:size puzzle)
        goal (solved size)]
    (reduce + (for [y (range size)
                    x (range size)
                    :let [case (get (:puzzle puzzle) (+ (* y size) x))
                          igoal (.indexOf goal case)
                          [gx gy] [(rem igoal size) (quot igoal size)]]
                    :when (not= 0 case)]
                (h [x y] [gx gy])))))

(def euclidian-distance
  "Returns euclidian distance using 2 sets of 2D coords"
  (memoize (fn [p q]
             (->> (map - p q)
                  (map #(* % %))
                  (reduce +)
                  (Math/sqrt)))))

(def manhattan-distance
  "Returns manhattan distance using 2 sets of 2D coords"
  (memoize (fn [p q]
             (->> (map - p q)
                  (map #(Math/abs %))
                  (reduce +)))))

(defn A*
  "Finds the shortest path using greedy search with a heuristic function
  assumes the puzzle is solvable"
  [puzzle heuristic]
  (let [size (:size puzzle)
        start (:puzzle puzzle)
        goal (solved size)
        h  #(board-heuristic {:size size :puzzle %} heuristic)]
    (loop [open (p/priority-map start 0)
           frontier []
           closed #{}
           from {}
           shortest {start 0}]
      (let [node (key (peek open))]
        (if (= node goal)
          {:size size
           :solution (vec (reverse (take-while (comp not nil?) (iterate from goal))))
           :moves (count (vec (reverse (take-while (comp not nil?) (iterate from goal)))))
           :frontier frontier}
          (let [g (shortest node)
                moves (remove closed (valid-moves {:size size :puzzle node}))
                hmoves (for [move moves
                             :let [hmove (h move)
                                   dist (shortest move Double/POSITIVE_INFINITY)]
                             :when (< (inc g) dist)]
                         hmove)]
            (recur (into (dissoc open node) (map (fn [a b] [a b]) moves hmoves))
                   (conj frontier (count open))
                   (conj closed node)
                   (into from (map (fn [m] [m node]) moves))
                   (into shortest (map (fn [m] [m (inc g)]) moves)))))))))
