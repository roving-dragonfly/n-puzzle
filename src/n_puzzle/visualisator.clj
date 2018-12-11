(ns n-puzzle.visualisator
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def step-bar 30)

(def window
  [900
   (+ 900 step-bar)])

(defn draw-tile
  "Draws a tile of size s numbered n at a pos [x y]"
  [x y n [sizex sizey]]
  (let [posx (* x sizex)
        posy (* y sizey)]
    (q/fill 0 100 100)
    (q/rect posx posy sizex sizey 10)
    (q/fill 0 0 100)
    (q/text-align :center :center)
    (q/text-size (* 0.5 sizex))
    (q/text (str n) (+ posx (* 0.5 sizex)) (+ posy (* 0.5 sizey)))))

(defn draw-step
  "Draws the solution step  bar under the puzzle"
  [state]
  (q/fill 0 100 0)
  (q/rect 0 (- (second window) step-bar)
          (* (inc (:step state)) (/ (first window) (count (:solution state))))
          step-bar)
  (q/fill 255 0 0)
  (q/text-align :center :center)
  (q/text-size (* 0.5 step-bar))
  (q/text (str (:step state))
          (* 0.5 (first window))
          (+ (- (second window) step-bar)
             (* 0.5 step-bar))))

(defn setup
  "Setup the state, used once"
  [p-size solution]
  {:solution solution
   :n p-size
   :tile-size [(/ (first window) p-size) (/ (- (second window) 30) p-size)]
   :step 0})

(defn draw
  "Only impure function in the process, no return value"
  [state]
  (let [board (nth (:solution state) (:step state))
        n (:n state)]
    (q/background 0)
    (draw-step state)
    (doseq [tile board
            :let [i (.indexOf board tile)]
            :when ((comp not zero?) tile)]
      (draw-tile (rem i n) (quot i n) tile
                 (:tile-size state)))))

(defn step
  "Changes state step"
  [state event]
  (let [key (:raw-key event)
        step (:step state)
        solutions (count (:solution state))]
    (cond 
      (and (= key \n)
           (< (inc step) solutions))
      (assoc state :step (inc step))
      (and (= key \p)
           (>= (dec step) 0))
      (assoc state :step (dec step))
      :else state)))

(defn draw-puzzle
  "Draws navigable solution"
  [solution]
  (q/defsketch puzzle
    :title "N-puzzle"
    :size window
    :setup #(setup (:size solution) (:solution solution))
    :draw draw
    :key-typed step
    :middleware [m/fun-mode]
    :features [:no-bind-output]))
