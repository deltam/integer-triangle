(ns integer-triangle.draw
  (:require [integer-triangle.core :as cr]
            [integer-triangle.heronian :as hr]
            [quil.core :as q :include-macros true]))

(defn centering
  "外接円の中心に合わせる"
  [t xys]
  (let [[ocx ocy] (cr/outer-center-xy t)]
    (map (fn [[x y]] [(- x ocx) (- y ocy)])
         xys)))

(defn draw-triangle [t]
  (->> (cr/points t)
       (centering t)
       (flatten)
       (apply q/triangle)))

(defn draw-diameter [t]
  (let [[[x y] _ _] (cr/points t)
        [ocx ocy] (cr/outer-center-xy t)
        cx (- x ocx)
        cy (- y ocy)]
    (q/line [cx cy] [(- cx) (- cy)])))

(defn draw-diagonal [t]
  (let [[[x y] _ _] (cr/points t)
        [ocx ocy] (cr/outer-center-xy t)
        cx (- x ocx)
        cy (- y ocy)]
    (q/line [(- cx) cy] [(- cx) (- cy)])))

(defn draw-diagonal [t]
  (let [[[x y] _ _] (cr/points t)
        [ocx ocy] (cr/outer-center-xy t)
        cx (- x ocx)
        cy (- y ocy)]
    (q/line [(- cx) cy] [(- cx) (- cy)])))

(defn draw-head [t]
  (let [[[x y] _ _] (cr/points t)
        [ocx ocy] (cr/outer-center-xy t)
        cx (- x ocx)
        cy (- y ocy)]
    (q/line [cx cy] [(- cx) cy])))

(defn draw-trapezoid-triangle [[t base-ang]]
  (let [tt (hr/triangle->trapezoid-parts t)
        [_ r l] (:side t)
        ang (if (< l r)
              (q/acos (cr/cos (cr/rotate t)))
              (- (q/acos (cr/cos (cr/rotate (cr/rotate t))))))
        ]
    (q/with-rotation [(+ base-ang ang)]
      (draw-triangle tt))
    [tt (+ base-ang ang)]
    ))

(defn draw-trapezoid [tp]
  (let [[[ax ay] p3 p4] (cr/points (:src tp))
        btm (cr/bottom (:src tp))
        mn (min ax (- btm ax))
        mx (max ax (- btm ax))]
    (->> [[mx ay] [mn ay] p3 p4]
         (centering (:src tp))
         (flatten)
         (apply q/quad))))

(defn draw-outer [ot angle]
  (q/with-rotation [angle]
;    (draw-triangle ot)
    ;(draw-diameter ot)
;    (q/with-fill [(q/color 200)])
;    (draw-triangle (cr/mirror ot))
    (let [tp (hr/triangle->trapezoid ot)]
      (draw-trapezoid tp)
      (let [ang (q/acos (hr/trapezoid-leg-cos tp))]
        (q/with-rotation [ang]
          (draw-trapezoid (hr/trapezoid->trapezoid tp)))
        (q/with-rotation [ang]
          (draw-trapezoid (hr/trapezoid->trapezoid tp)))))

                                        ;    (let [tp1 (hr/triangle->trapezoid ot)])

;    (doall (take 4 (iterate draw-trapezoid-triangle [(cr/mirror ot) 0.0])))

;    (draw-diagonal ot)
                                        ;  (draw-head ot)
    ))

(defn draw-all [t]
  (let [r (cr/outer-radius t)]
    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (q/ellipse 0 0 (* r 2) (* r 2))
 ;     (q/with-fill [120])
      (draw-triangle t)
      (q/no-fill)

      (draw-outer (hr/outer-triangle t) (q/radians 180))

      (draw-outer (hr/outer-triangle (cr/rotate t))
                  (- (q/acos (cr/cos (cr/rotate (cr/rotate t))))))

      (draw-outer (hr/outer-triangle (cr/rotate (cr/rotate t)))
                  (q/acos (cr/cos (cr/rotate t))))
      )))

(defn resize-with-outer-radius [t d]
  (let [r (cr/outer-radius t)]
    (cr/scalling-triangle t (/ d r))))

(defn draw []
  (q/background 255)
  (q/no-fill)
  (let [t1 (cr/tri [15 14 13] 84)
        t2 (cr/tri [15 13 14] 84)
        t3 (cr/tri [5 5 6] 12)
        t4 (cr/tri [29 25 6] 60)
        t5 {:side [15N 39/4 33/4], :area 297/8}
        t6 (cr/tri [21 20 13] 126)
        ]
    (draw-all (resize-with-outer-radius t1 350))
    ))

(q/defsketch my
  :host "host"
  :size [800 800]
  :draw draw)
