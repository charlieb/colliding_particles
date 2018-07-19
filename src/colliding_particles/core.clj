(ns colliding_particles.core
  (:use clojure.set)
  (:require [quil.core :as q]
            [quil.middleware :as m]))
(defn v- [[x1 y1] [x2 y2]] [(- x1 x2) (- y1 y2)])
(defn v+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defn v* [[x y] scl] [(* x scl) (* y scl)])
(defn mag-sq [[x y]] (+ (* x x) (* y y)))
(defn mag [v] (Math/sqrt (mag-sq v)))
(defn norm [[x y]] (let [m (mag [x y])] [(/ x m) (/ y m)]))
(defn rnd-v [] 
  (let [a (q/random 6.283)]
    [(q/cos a) (q/sin a)]))


(defn mk-part [] {:pos [0 0] :vel [0 0] :emitter nil :history '()})
(defn mk-emitter [] {:pos [0 0] :str 1. :col [1. 1. 1. 1.]})

(defn rnd-emitter [x y] 
  (assoc (mk-emitter)
         :pos [(q/random x) (q/random y)]
         :col [(q/random 1.) 0.8 0.8 0.3]))
(defn emit [e]
  (assoc (mk-part)
         :pos (v+ (:pos e) (rnd-v))
         :emitter e))


(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb 1.0)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (let [emitters (take 50 (repeatedly #(rnd-emitter 500 500)))
        particles (take 500 (repeatedly #(emit (rand-nth emitters))))]

  {:particles particles
   :emitters emitters
   :dead-particles '()
   }))


(defn update-dead-alive [state]
  (loop [p (first (:particles state))
         ps (rest (:particles state))
         dead #{}
         alive '()]
    (if (empty? ps)
      (assoc state
             :particles (concat (conj alive p)
                                (take (count dead)
                                      (repeatedly #(emit (rand-nth (:emitters state))))))
             :dead-particles (concat (:dead-particles state) dead))
    (let [hits (set 
                 (filter (fn [p2] (and (not (= (:emitter p) (:emitter p2)))
                                       (< (mag (v- (:pos p) (:pos p2))) 5))) 
                         ps))
          new-dead (if (empty? hits) #{} (conj hits p))
          left-alive (filter #(not (contains? hits %)) ps)]
      (recur (first left-alive)
             (rest left-alive)
             (union dead new-dead)
             (if (empty? new-dead) (conj alive p) alive))))))

(defn update-accelerations [state]
  (assoc state
         :particles
         (map (fn [p]
                (let [dv (v- (:pos p) (:pos (:emitter p)))
                      dvmag (min 5 (mag dv))
                      dvnorm (norm dv)]
                  (assoc p
                         :vel (v+ (:vel p)
                                  (v* dvnorm dvmag)))))
              (:particles state))))

(defn update-positions [state]
  ;(println 'positions state)
  (assoc state
         :particles
         (map #(assoc %
                      :pos (v+ (:pos %)
                               (:vel %)))
              (:particles state))))


(defn update-state [state]
  (->> state
       update-dead-alive
       update-accelerations
       update-positions))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;(q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  ;(println 'draw (map :pos (:particles state)))
  (println (count (:dead-particles state)))
  (doseq [p (:particles state)]
    (q/stroke 1.0 1.0 0.8 0.2)
    (q/ellipse ((:pos p) 0) ((:pos p) 1) 5 5)
    )
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
