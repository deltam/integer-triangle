(ns integer-triangle.pythagoras
  "ピタゴラス三角形"
  (:require [integer-triangle.core :as c]))


(defn not=odd [[a b]]
  (not= (mod a 2) (mod b 2)))

(def mn-pairs (->> c/all-pairs
                   (filter c/coprime?)
                   (filter not=odd)))

(defn mn->triangle [[m n]]
  (list (- (* m m) (* n n))
        (* 2 m n)
        (+ (* m m) (* n n))))

(def pythagorean-triangles (map mn->triangle mn-pairs))
