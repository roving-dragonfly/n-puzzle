(defproject n-puzzle "1.0"
  :description "Solver of taquin puzzle"
  :url "https://github.com/roving-dragonfly/n-puzzle"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.priority-map "0.0.10"]
                 [quil "2.8.0"]
                 [org.clojure/tools.cli "0.4.1"]]
  :main ^:skip-aot n-puzzle.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
