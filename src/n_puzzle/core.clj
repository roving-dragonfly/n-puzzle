(ns n-puzzle.core
  (:require [n-puzzle.solver :refer [solvable? A* euclidian-distance manhattan-distance]]
            [n-puzzle.parser :refer [parse]]
            [n-puzzle.generator :refer [generate-from-solved generate-puzzle]]
            [n-puzzle.visualisator :refer [draw-puzzle]]))

(defn file->map
  "Get parsed map from file"
  [file]
  (-> (slurp file)
      parse))

(defn full-process
  "Solves a puzzle map then draws its solution"
  [puzzle heuristic]
  (if (solvable? puzzle)
    (draw-puzzle (A* puzzle heuristic))
    (println "Not solvable")))

;; Some commands to tryout

(full-process (generate-puzzle 5) manhattan-distance)
