(ns pacman.core
  (:require [play-clj.core :as play-clj :refer :all]
            [play-clj.g2d :as g2d :refer :all]
            [play-clj.math :as math :refer :all]))

(def pac-size 40)
(def brick-size 90)

(defn- gen-bricks [quantity, generate-bricks-fn]
  (while (< 0 quantity)
    [x (- 800 (* 100 quantity)) y (- 450 (* 80 quantity))]
    (generate-bricks-fn x y)
    (dec quantity)))

;;|--------------- bricks -----------------|
(defn- gen-bricks
  "This function generate entities of bricks with a matrix-like position"
  [brick-size, generate-bricks-fn]
  (for [x (range 180 800 240)
        y (range 80 450 160)]
    (generate-bricks-fn x y brick-size)))

(defn- check-position [new-x new-y [{:keys [brick?] :as entity}]]
  (if brick?
    (or (= new-x (get entity :x))
        (= new-y (get entity :y)))
    entity))

(comment
  (check-position 10 20 [{:brick? true :x 10 :y 20}])
  ;; => true
  (check-position 11 21 [{:brick? true :x 10 :y 20}])
  ;; => false
  )

;;|--------------- dots -----------------|
(defn- gen-dots
  "This function generate entities of dot with a matrix-like position"
  [dot-size generate-dot-fn]
  (for [x (range 100 800 40)
        y (range 80 450 80)]
    (generate-dot-fn x y dot-size)))

;;|-------------------- handle input --------------------------|
(defn- get-direction []
  (cond
    (play-clj/key-pressed? :dpad-right) :right
    (play-clj/key-pressed? :dpad-left) :left
    (play-clj/key-pressed? :dpad-up) :up
    (play-clj/key-pressed? :dpad-down) :down))

;;|--------------- handle player position -----------------|

#_(defn- get-new-x [direction entity]
    (let [new-x (case direction
                  :left (- (:x entity) 15)
                  :right (+ (:x entity) 15)
                  (:x entity))]
      (if (or (< new-x 0) (<= (- 900 (:width entity)) new-x)) ;apply screen x boundaries
        (:x entity)
        new-x)))
#_
(defn- get-new-y [direction entity]
  (let [new-y (case direction
                :up (+ (:y entity) 15)
                :down (- (:y entity) 15)
                (:y entity))]
    (if (or (< new-y 0) (<= (- 500 (:height entity)) new-y)) ;apply screen y boundaries
      (:y entity)
      new-y)))

;; (def movements
;;   {:x {:max-size 900 :movements {:up 0 :down 0 :left -15 :right 15}}
;;    :y {:max-size 500 :movements {:up 15 :down -15 :left 0 :right 0}}})

;; (defn update-pos [value x-or-y direction size]
;;   (let [{:keys [max-size movements]} (get movements x-or-y)]
;;     (-> value
;;         (+ (get movements direction 0))
;;         (clamp 0 (- max-size size)))))

;; (defn- get-new-x [direction {width :width :as entity}]
;;   (update entity :x (fn [value] (update-pos value :x direction width)))
;;   (update entity :x update-pos :x direction width))

;; (defn- get-new-y [direction {height :height :as entity}]
;;   (update entity :y update-pos :y direction height))

;; (comment
;;   (get-new-x :left {:x 10 :width 500})
;;   (get-new-x :right {:x 100 :width 500})
;;   (get-new-y :down {:x 100 :y 300 :width 500 :height 800})
;;   )

(defn clamp
  "Limit value to a certain range (min/max)"
  [val min-val max-val]
  (min (max min-val val) max-val))

(def movements
  {:right [:x 15 :width]
   :left  [:x -15 :width]
   :up    [:y 15 :height]
   :down  [:y -15 :height]})

(def max-dimensions
  {:width 900
   :height 500})

(defn update-position [{:keys [direction] :as entity}]
  (let [[x-or-y offset dimension] (get movements direction)]
    (-> entity
        (update x-or-y + offset)
        (update x-or-y clamp 0 (- (get max-dimensions dimension)
                                  (get entity dimension))))))

(comment
  (update-position {:x 10 :y 20 :width 100 :height 100 :direction :right})
  (update-position {:x 10 :y 20 :width 100 :height 100 :direction :left})
  (update-position {:x 780 :y 20 :width 100 :height 100 :direction :right})
  (update-position {:x 790 :y 20 :width 100 :height 100 :direction :right})
  ;; => (-> entity
  ;;        (update :x + 15)
  ;;        (update :x clamp 0 (- (get max-dimensions :width)
  ;;                              (get entity :width))))
  (update-position {:x 10 :y 20} :right)
  )

(defn- get-angle [direction]
  (case direction
    :right 0
    :up    90
    :left  180
    :down  270))

;; adding new player entity
(defn- update-player-position [{:keys [player?] :as entity}]
  (if player?
    (let [direction (get-direction)
          angle (get-angle direction)]
      (-> entity
          (assoc :direction direction :angle angle)
          (update-position)))
    entity))


(defn- update-collected-list [{:keys [player? dot?] :as entity}]
  (if (or player? dot?)
    (assoc entity :hit-box
           (rectangle (:x entity) (:y entity) (:width entity) (:height entity)))
    entity))

(defn- remove-collected-dots [entities]
  (if-let [dots (filter #(contains? % :dot?) entities)]   ;get only dot entities
    (let [player (some #(when (:player? %) %) entities)   ;some returns the first logical true!
          touched-dots (filter #(rectangle! (:hit-box player) :overlaps (:hit-box %)) dots)] ;use rectangle! because we already have a rectangle and we want to call a function on it
      (remove (set touched-dots) entities))
    entities))


(defn- move-and-collect [entities]
  (def xxx entities)
  (->> entities #_(update-in entities [1 :x] + 15)

       (map (fn [entity]
              (->> entity
                   (update-player-position)
                   (update-collected-list))))
       (remove-collected-dots)))


(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (let [
          background (texture "background.png")
          player (assoc (texture "pac.png")
                        :player? true :x 40 :y 40
                        :width pac-size :height pac-size
                        :angle 0 :direction '(:right :left :up :down))
          generate-dot-fn (fn [x y size]
                            (assoc (texture "dot.png")
                                   :dot? true
                                   :x x :y y
                                   :width size :height size)
                            )
          dots (gen-dots 20 generate-dot-fn)

          generate-bricks-fn (fn [x y size]
                               (assoc (texture "big-wall.png")
                                      :brick? true
                                      :x x :y y
                                      :width size))
          bricks (gen-bricks brick-size generate-bricks-fn)]

      [background player dots bricks])
    )                                            ; end of :on-show function

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entities]
    (try
      (cond
        (get-direction) (move-and-collect entities))
      (catch Exception e
        (def last-exception e)
        (println e)
        entities)))
  )                                              ; end of defscreen

(defgame pacman-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
