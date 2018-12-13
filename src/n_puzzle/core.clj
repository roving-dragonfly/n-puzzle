(ns n-puzzle.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :refer [as-file]]
            [n-puzzle.solver :refer [solvable?]]
            [n-puzzle.parser :refer [parse]]
            [n-puzzle.generator :refer [generate-from-solved generate-puzzle]]
            [n-puzzle.visualisator :refer [draw-puzzle]])
  (:gen-class))


(defn print-puzzle
  "Prints a puzzle state"
  [puzzle]
  (let [lines (partition (:size puzzle) (:puzzle puzzle))]
    (doseq [line lines]
      (println (map #(format "%2d" %) line)))))

(defn print-solution
  "Prints a solution"
  [solution]
  (println (str "Solution took " (:moves solution) " moves"))
  (println (str "Frontier expanded to " (apply max (:frontier solution))) "potentials paths")
  (println "Solution from initial state to solved state")
  (doseq [puzzle (:solution solution)]
    (print-puzzle {:size (:size solution) :puzzle puzzle})
    (println "")))

(def cli-options
  (let [algos ["A*"]
        heuristics ["manhattan-distance" "euclidian-distance"]] 
    [["-a" "--algorithm ALGORITHM" "Algorithm used"
      :default "A*"
      :validate [(fn [e] (prn e) (some #(= % e) algos)) "This is not a valid algorithm"]]
     ["-h" "--heuristic HEURISTIC" "Heuristic used"
      :default "manhattan-distance"
      :validate [(fn [e] (prn e) (some #(= % e) heuristics)) "This is not a valid heuristic"]]
     ["-v" "--visualisator" "Solution visualisator"
      :default true
      :parse-fn not]
     ["-m" "--map MAP" "Input map"
      :default nil]]))

(defn -main [& args]
  (let [args (parse-opts args cli-options)
        filename (get-in args [:options :map])
        file (when (and filename (.exists (as-file filename))) (slurp filename))]
    (if-let [err (:errors args)]
      (prn err)
      (let [map (if file (parse file) (generate-puzzle 4))
            algo  (ns-resolve (find-ns 'n-puzzle.solver)
                              (symbol (get-in args [:options :algorithm])))
            heuristic  (ns-resolve (find-ns  'n-puzzle.solver)
                                   (symbol (get-in args [:options :heuristic])))
            solution (when (solvable? map) (algo map heuristic))]
        (cond
          (nil? solution) (println "Not solvable")
          (get-in args [:options :visualisator]) (draw-puzzle solution)
          :else (print-solution solution))))))

