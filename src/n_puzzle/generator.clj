(ns n-puzzle.generator)

;; Direction vectors of the desired solution
(def snail-vectors
  [[1 0]
   [0 1]
   [-1 0]
   [0 -1]])

(def solved
  "Returns a snail state puzzle of size n"
  (memoize
   (fn [n]
     (loop [cases (conj (vec (range 1 (* n n))) 0)
            puzzle (vec (repeat (* n n) nil))
            moves (mapcat identity (repeat snail-vectors))
            [x y] [0 0]]
       (let [[dx dy] (mapv + [x y] (first moves))
             next-move (if (or (not (and (< -1 dx n) (< -1 dy n)))
                               (get puzzle (+ (* dy n) dx)))
                         (rest moves)
                         moves)]
         (if (nil? (seq cases))
           puzzle
           (recur (rest cases)
                  (assoc puzzle (+ (* y n) x) (first cases))
                  next-move
                  (mapv + [x y] (first next-move)))))))))

(defn generate-puzzle
  "Generate a random puzzle of size n"
  [n]
  (let [cases (range (* n n))
        shuffled (shuffle cases)
        puzzle (vec shuffled)]
    {:size n
     :puzzle puzzle}))

(defn valid-moves
  "Returns coll of valid boards from state"
  [puzzle]
  (let [size (:size puzzle)
        board (:puzzle puzzle)
        hole-index (.indexOf board 0)
        [hx hy] [(rem hole-index size) (quot hole-index size)]]
    
    (for [[x y] [[1 0] [0 1] [-1 0] [0 -1]]
          :let [index (+ (* (+ hy y) size) (+ hx x))]
          :when (and (< -1 (+ hx x) size)
                     (< -1 (+ hy y) size))]
      (assoc board
             index (board hole-index)
             hole-index (board index)))))

(defn generate-from-solved
  "Generates a n-puzzle using random valid moves from solved state"
  [n perm]
  (nth (iterate (fn [p]
                  {:size n
                   :puzzle (rand-nth (valid-moves p))})
                {:size n
                 :puzzle (solved n)})
       perm))
