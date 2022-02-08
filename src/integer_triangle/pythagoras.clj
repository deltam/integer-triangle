(ns integer-triangle.pythagoras
  "ピタゴラス三角形"
  (:require [integer-triangle.core :as c]))


(defn not=odd [[a b]]
  (not= (mod a 2) (mod b 2)))

(def mn-pairs (->> c/all-pairs
                   (filter c/coprime?)
                   (filter not=odd)))

(defn mn->triangle [[m n]]
  (let [[a b c] (sort > [(- (* m m) (* n n))
                         (* 2 m n)
                         (+ (* m m) (* n n))])]
    (c/tri [a b c] (* b c 1/2))))

(def pythagorean-triangles (map mn->triangle mn-pairs))




(comment

(->> py/pythagorean-triangles
     (map (fn [p]
            (let [[a b c] (:side p)]
              (assoc p
                     :n (int (Math/floor (/ (* 2 Math/PI) (q/acos (cos {:side [c b a]})))))
                     :mid (/ (:area %) (sq a))))))
     (filter #(< 3.05 (* (:n %) (:mid %)) Math/PI))
     (take 3)
     )

;=> ({:side [113 112 15], :area 840N, :n 47, :mid 840/12769}
;=>  {:side [145 144 17], :area 1224N, :n 53, :mid 1224/21025}
;=>  {:side [181 180 19], :area 1710N, :n 59, :mid 1710/32761})

)
