(ns colliding_particles.core
  (:use clojure.set)
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn mk-part [] {:pos [0 0] :vel [0 0] :type 0 :history '()})
(defn mk-emitter [] {:pos [0 0] :str 1. :col [1. 1. 1. 1.] :type 0})

(defn rnd-emitter [x y] 
  (assoc (mk-emitter)
         :pos [(q/random x) (q/random y)]
         :col [(q/random 1.) 0.8 0.8 0.3]))
(defn emit [e]
  (assoc (mk-part)
         :pos [((:pos e) 0) ((:pos e) 1)]
         :type (:type e)))
(defn v- [[x1 y1] [x2 y2]] [(- x1 x2) (- y1 y2)])
(defn mag-sq [[x y]] (+ (* x x) (* y y)))
(defn mag [v] (Math/sqrt (mag-sq v)))
(defn norm [[x y]] (let [m (mag [x y])] [(/ x m) (/ y m)]))


(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb 1.0)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (let [emitters (map-indexed (fn [i e] (assoc e :type i))
                              (take 5 (repeatedly #(rnd-emitter 500 500))))
        particles (take 100 (repeatedly #(emit (rand-nth emitters))))]

  {:nparticles 100
   :particles particles
   :emitters emitters
   :dead-particles '()
   }))

(defn update-state [state]
  (loop [p (first (:particles state))
         ps (rest (:particles state))
         dead #{}
         alive '()]
    (if (empty? ps)
      (assoc state
             :particles (concat (conj alive p)
                                (take (- (:nparticles state) (count alive) -1) ; correct for remaining p
                                      (repeatedly #(emit (rand-nth (:emitters state))))))
             :dead-particles (concat (:dead-particles state) dead))
    (let [hits (set 
                 (filter (fn [p2] (and (not (= (:type p) (:type p2)))
                                       (< (mag (v- (:pos p) (:pos p2))) 5))) 
                         ps))
          new-dead (if (empty? hits) #{} (conj hits p))
          left-alive (filter #(not (contains? hits %)) ps)]
      (recur (first left-alive)
             (rest left-alive)
             (union dead new-dead)
             (if (empty? new-dead) (conj alive p) alive))))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;(q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (println (map :pos (:particles state)))
  )

(defn -main [& args]
  (q/defsketch colliding_particles
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
