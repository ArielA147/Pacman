(ns pacman.core.desktop-launcher
  (:require [pacman.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. pacman-game "pacman" 900 500)
  (Keyboard/enableRepeatEvents true))
