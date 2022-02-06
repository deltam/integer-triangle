(ns integer-triangle.heronian
  (:require [integer-triangle.core :as cr]))

;;; ヘロンの三角形

(defn coprime3? [[a b c]]
  (and (cr/coprime? [a b])
       (cr/coprime? [b c])
       (cr/coprime? [c a])))

(defn mn->mnk [[m n]]
  (let [mn (* m n)]
    (->> (range 1 mn)
         (filter #(> mn (* % %)))
         (map #(conj [m n] %)))))

(defn mnk-prod [[m n k]]
  (>= (* k k) (/ (* m m n) (+ (* 2 m) n))))

(def mnk-triples (->> cr/all-pairs
                      (mapcat mn->mnk)
                      (filter coprime3?)
                      (filter mnk-prod)))

(defn mnk->triangle [[m n k]]
  (let [mm (* m m)
        nn (* n n)
        kk (* k k)]
    (tri [(* n (+ mm kk))
          (* m (+ nn kk))
          (* (+ m n) (- (* m n) kk))]
         (* m n k (+ m n) (- (* m n) (* k k))))))

(def heronian-triangles (->> mnk-triples
                             (map mnk->triangle)))





;; Brahmaguputa

(defn split-triangle [{[a b c] :side, s :area}]
  (let [h (cr/height (tri [a b c] s))
        a1 (* b (cr/cos {:side [c a b]}))
        a2 (* c (cr/cos {:side [b c a]}))]
    [(tri [a1 b h] (* a1 h 1/2))
     (tri [a2 c h] (* a2 h 1/2))]))

(defn merge-triangles [[t1 t2]]
  (let [[m1 _ _] (:side t1)
        [m2 _ _] (:side t2)
        [_ b2 c2] (map #(* % m2) (:side t1))
        [_ e2 f2] (map #(* % m1) (:side t2))
        area1 (* (:area t1) m2 m2)
        area2 (* (:area t2) m1 m1)]
    (tri [(+ c2 f2) e2 b2] (+ area1 area2))))

(defn outer-triangle
  "辺と外接円の隙間に接する外縁三角形"
  [t]
  (let [ot (-> t
               (split-triangle)
               (merge-triangles))]
    (cr/scalling-triangle ot (/ (cr/bottom t) (cr/bottom ot)))))



(defn corner-cos
  "底辺の左右の角のcos"
  [{[a b c] :side}]
  [(cr/cos (tri [b c a] 0))
   (cr/cos (tri [c b a] 0))])


;; trapezoid

(defn triangle->trapezoid
  "鏡映を使って等脚台形をつくる"
  [t]
  (let [[a b c] (:side t)
        b2 (max b c)
        c2 (min b c)
        head (/ (- (* b2 b2) (* c2 c2)) a)]
    {:src t, :head head
     :area (* (+ a head) (cr/height t) 1/2)}))

(defn trapezoid-diff
  "鏡映を使った等脚台形と元三角形の差分三角形"
  [tp]
  (let [[a b c] (:side (:src tp))
        mx (max b c)
        mn (min b c)
        s (- (:area tp) (:area (:src tp)))]
    (if (< b c)
      (tri [mx (:head tp) mn] s)
      (tri [mx mn (:head tp)] s))))

(defn trapezoid->trapezoid
  "等脚台形の斜辺に貼り付く等脚台形を返す"
  [tp]
  (-> tp
      (trapezoid-piece)
      (triangle->trapezoid)
      (trapezoid-piece)
      (triangle->trapezoid)))

(defn trapezoid-leg-cos
  "等脚台形の下底と脚の角のcos"
  [tp]
  (apply min (corner-cos (:src tp))))

(defn trapezoid-format [tp]
  (let [btm (cr/bottom (:src tp))
        up (:head tp)
        [_ r l] (:side (:src tp))
        leg (min r l)
        diag (max r l)]
    (println btm up leg diag (:area tp))))

(defn triangle->trapezoid-parts [{[a b c] :side, s :area}]
  (let [b_ (max b c)
        c_ (min b c)
        u (/ (- (* b_ b_) (* c_ c_)) a)
        h (/ s a 1/2)
        s_ (* (+ a u) h 1/2)]
    (if (< b c)
      (tri [b_ u c_] (- s_ s))
      (tri [b_ c_ u] (- s_ s)))))


(defn map-all-angle [f t]
  (map f
       [t
        (cr/rotate t)
        (cr/rotate (cr/rotate t))]))


(def samples
  (->> [[5	4	3]
        [6	5	5]
        [8	5	5]
        [15	13	4]
        [13	12	5]
        [17	10	9]
        [26	25	3]
        [20	15	7] [13	13	10] [17	15	8] [24	13	13] [29	25	6] [20	13	11] [30	29	5] [15	14	13] [21	17	10] [25	24	7] [35	29	8] [25	17	12] [53	51	4] [37	20	19] [17	17	16] [30	17	17] [39	25	16] [21	20	13] [41	28	15] [52	51	5] [30	25	11] [37	26	15] [51	40	13] [25	25	14] [39	35	10] [48	25	25] [37	30	13] [41	40	9] [65	55	12] [26	25	17] [29	21	20] [28	25	17] [39	28	17] [37	35	12] [68	65	7] [149	148	3] [80	73	9] [52	41	15] [40	37	13] [35	34	15] [45	40	13] [70	65	9] [44	37	15] [65	34	33] [52	29	27] [80	65	17] [74	51	25] [123	122	5] [51	37	20] [44	39	17] [52	33	25] [61	60	11] [109	100	11] [41	40	17] [53	35	24] [61	52	15] [195	193	4] [36	29	25] [41	41	18] [80	41	41] [75	68	13] [87	55	34] [97	90	11] [120	109	13]]
      (map #(tri % (int (cr/sides->area %))))))


(comment

(let [t (tri [15 14 13] 84)]
  (->> t
       (hr/map-all-angle hr/outer-triangle)
       (map :area)
       (apply +))
;=> 84N

  (->> t
       (hr/map-all-angle hr/outer-triangle)
       (map hr/triangle->trapezoid-parts)
       (map :area)
       (apply +))
;=> 65463/4225

  (->> t
       (hr/map-all-angle hr/outer-triangle)
       (map hr/triangle->trapezoid-parts)
       (map hr/triangle->trapezoid-parts)
       (map :area)
       (apply +)
       (* 2))
;=> 236692806/17850625

  (->> t
       (hr/map-all-angle hr/outer-triangle)
       (map hr/triangle->trapezoid-parts)
       (map hr/triangle->trapezoid-parts)
       (map hr/triangle->trapezoid-parts)
       (map :area)
       (apply +)
       (* 2))
;=> 1823729811040224/318644812890625

  (+ 84 84 65463/4225 236692806/17850625 1823729811040224/318644812890625)
;=> 64518319733753349/318644812890625

  (double (+ 84 84 65463/4225 236692806/17850625 1823729811040224/318644812890625))
;=> 202.4772320894466

  (double (/ (+ 84 84 65463/4225 236692806/17850625 1823729811040224/318644812890625) (* 65/8 65/8)))
;=> 3.067110734609368

  )
)
