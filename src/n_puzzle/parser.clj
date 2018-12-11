(ns n-puzzle.parser
  (:require [clojure.string :as s]))

(defn trim-coll
  "Removes all empty strings from a coll"
  [coll]
  (filter (comp not s/blank?) coll))

(defn uncomment
  "Removes comments from coll of lines"
  [lines]
  (let [no-comment (map #(s/replace % #"#.*" "") lines)]
    (trim-coll no-comment)))

(defn str->int
  "Converts string to integer, throws if bad input"
  [str]
  (try (Integer. str)
       (catch Exception e
         (throw (ex-info "Input file must contains only numbers"
                         {:exception e})))))

(defn tokenize
  "Create token list from puzzle lines"
  [coll]
  (let [words (map #(s/split % #" +") coll)
        cleaned (map trim-coll words)
        tokens (map #(map str->int %) cleaned)]
    tokens))

(defn valid-map
  "Checks if the map is valid"
  [tokens]
  (let [n (and (= 1 (count (first tokens))) (first (first tokens)))
        rows (map count (rest tokens))]
    (if (or (not n)
            (some #(not= n %) rows)
            (not= (count (rest tokens)) n))
      (throw (ex-info "Invalid map dimentions"
                      {:n n :map (rest tokens)})))
    (if (not= (sort (flatten (rest tokens)))
              (range (* n n)))
      (throw (ex-info "Invalid cells numbers"
                      {:cells (sort (flatten (rest tokens)))})))
    {:size n
     :puzzle (vec (flatten (rest tokens)))}))

(defn parse
  "Parses a n-puzzle string"
  [puzzle]
  (-> puzzle
      s/split-lines
      uncomment
      tokenize
      valid-map))
