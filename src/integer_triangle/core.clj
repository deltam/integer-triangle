(ns integer-triangle.core)

(defn lowers [n] (range 1 (inc n)))
(defn low-pairs [n]
  (map #(list n %)
       (lowers n)))
(defn num-pairs [n]
  (->> (iterate inc n)
       (mapcat low-pairs)))

(def ^{:doc "自然数のすべてのペア"}
  all-pairs (num-pairs 1))

;(sp/fdef gcd
;  :args (sp/(int? :a) (int? :b))
;  :ret int?
;  )

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn coprime? [[a b]]
  (= 1 (gcd a b)))



;;; tools

(defn sides->area
  "ヘロンの公式で三辺の長さから面積を出す"
  [[a b c]]
  (let [s (/ (+ a b c) 2.0)]
    (Math/sqrt (* s (- s a) (- s b) (- s c)))))

(defn tri
  "三角形を表す。底辺、右、左の順"
  [[a b c] s]
  {:side [a b c]
   :area s})

(defn bottom [t]
  (first (:side t)))

(defn rotate
  "左辺を底辺にする"
  [{[a b c] :side, s :area}]
  (tri [c a b] s))

(defn height
  "底辺に向かい合う角からの垂線の長さ"
  [{[bottom _ _] :side, s :area}]
  (/ s bottom 1/2))

(defn cos
  "底辺に向かい合う角の余弦"
  [{[a b c] :side}]
  (/ (- (* a a) (* b b) (* c c))
     (- (* 2 b c))))


(defn points
  "三角のxy座標"
  [t]
  (let [h (height t)
        [a b c] (:side t)
        cs (cos (rotate t))]
    [[(- a (* b cs)) (- h)] [0 0] [a 0]]))

(defn scalling-triangle
  "三角形をsc倍する"
  [t sc]
  (-> t
      (update :side (fn [sd] (mapv #(* % sc) sd)))
      (update :area #(* % sc sc))))

(defn mirror
  "鏡映"
  [{[a b c] :side, s :area}]
  (tri [a c b] s))





;; center

(defn- sq [x] (* x x))

(defn- multiply-xy [[x y] k]
  [(* k x) (* k y)])

(defn- reduce-xy [xys]
  (reduce (fn [[ax ay] [x y]] [(+ ax x) (+ ay y)])
          [0 0]
          xys))

(defn- weighted-avg [xys ks]
  (let [sum (apply + ks)]
    (->> ks
         (map multiply-xy xys)
         (reduce-xy)
         (map #(/ % sum)))))

(defn median-center-xy
  "重心のxy座標"
  [t]
  (weighted-avg (points t)
                (take 3 (repeat 1)) ; ただの平均
                ))

(defn outer-radius
  "外接円の半径"
  [{[a b c] :side, s :area}]
  (/ (* a b c) (* 4 s)))

(defn outer-center-xy
  "外心の座標"
  [t]
  (let [sqside (map sq (:side t))
        sqsum (apply + sqside)
        ks (map #(* % (- sqsum % %)) sqside)]
    (weighted-avg (points t) ks)))

(defn inner-radius
  "内接円の半径"
  [t]
  (let [pm (apply + (:side t))]
    (/ (* 2 (:area t)) pm)))

(defn inner-center-xy
  "内心の座標"
  [t]
  (weighted-avg (points t) (:side t)))
