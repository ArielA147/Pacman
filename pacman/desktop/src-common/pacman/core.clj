(ns pacman.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]))

(def pac-size 40)
(def brick-size 90)

;;|--------------- bricks -----------------|
(defn- gen-bricks [brick-size, generate-bricks-fn]
  "This function generate entities of bricks with a matrix-like position"

  ; (defn- gen-bricks [quantity, generate-bricks-fn]
    ;(while (< 0 quantity) (do [x (- 800 (* 100 quantity)) y (- 450 (* 80 quantity))] (generate-bricks-fn x y) (dec quantity)))
    (for [x (range 180 800 240) y (range 80 450 160)]
      (generate-bricks-fn x y brick-size))
    )

(defn- check-positoin [new-x new-y [{:keys [brick?] :as entity}]]
  (if brick?
    (or (= new-x :x entity) (= new-y :y entity))
    entity)
  )
  ;|--------------- dots -----------------|
  (defn- gen-dots [dot-size generate-dot-fn]
    "This function generate entities of dot with a matrix-like position"
    (for [x (range 100 800 40) y (range 80 450 80)]
      (generate-dot-fn x y dot-size)
      )
    )

  ;|-------------------- handle input --------------------------|
  (defn- get-direction []
    (cond
      (key-pressed? :dpad-right) :right
      (key-pressed? :dpad-left) :left
      (key-pressed? :dpad-up) :up
      (key-pressed? :dpad-down) :down
      )
    )

  ;|--------------- handle player position -----------------|

  (defn- get-new-x [direction entity]
    (let [new-x (case direction
                  :left (- (:x entity) 15)
                  :right (+ (:x entity) 15)
                  (:x entity))]
      (if (or (< new-x 0) (<= (- 900 (:height entity)) new-x)) ;apply screen y boundaries
        (:y entity)
        new-x)

      ))

  (defn- get-new-y [direction entity]
    (let [new-y (case direction
                  :up (+ (:y entity) 15)
                  :down (- (:y entity) 15)
                  (:y entity))]
      (if (or (< new-y 0) (<= (- 500 (:height entity)) new-y)) ;apply screen y boundaries
        (:y entity)
        new-y)))

  (defn- get-angle [direction]
    (case direction
      :right 0
      :up 90
      :left 180
      :down 270
      )
    )

  ; adding new player entity
  (defn- update-player-position [{:keys [player?] :as entity}]
    (if player?
      (let [direction (get-direction)
            x (get-new-x direction entity)
            y (get-new-y direction entity)
            angle (get-angle direction)
            ]
        (assoc entity :x x :y y :direction direction :angle angle)
        )
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
    (->> entities
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
                     bricks (gen-bricks brick-size generate-bricks-fn)
                     ]

                 [background player dots bricks])
               )                                            ; end of :on-show function

             :on-render
             (fn [screen entities]
               (clear!)
               (render! screen entities))

             :on-key-down
             (fn [screen entities]
               (cond
                 (get-direction) (move-and-collect entities)))
             )                                              ; end of defscreen

  (defgame pacman-game
           :on-create
           (fn [this]
             (set-screen! this main-screen)))