(ns day14-part2
  (:require [clojure.string :as s]))
;;copied from my clojure code base. not copying the input file

(def lines (s/split-lines (slurp "resources/someinput")))

(def string-points (map #(s/split % #" -> ") lines))

(defn point-to-int-array [x]
  "x is a list of 'x_pos,y_pos' strings"
  "result is a list of (x_posint y_posint)"
  (map #(Integer/parseInt %) (s/split x #",")))

(defn list-of-points-to-int-arrays [x]
  "x is a list of lists of 'x_pos,y_pos'"
  "results is a list of lists of (x_posint y_posint)"
  (map point-to-int-array x))

(def input (map list-of-points-to-int-arrays string-points))

(def sample-cave
  [[:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]
   [:air :air :air :air :air :air :air :air :air :air :air :air :air]])

(def sample-lines
  [[[4,4], [4,6], [2, 6]]
   [[9,4], [8,4], [8,9], [0,9]]
   ])

(def sample-lines2
  [[[9,4], [9, 6], [7,6]],
   [[14,4], [13,4], [13,9], [5,9]]])

(def my-cave
  (let [row (repeat 671 :air)
        table (vec (repeat 170 (vec row)))
        floor (repeat 671 :rok)]
    (into table [(vec floor)])))

(defn update-cave [cave x y v]
  (let [row (cave y)
        row-updated (update row x (fn [_] v))]
    (update cave y (fn [_] row-updated))))

(defn draw-vertically [cave x fromy toy]
  (if (> fromy toy)
    (draw-vertically cave x toy fromy)
    (let [cave1 (update-cave cave x fromy :rok)]
      (if (= fromy toy)
        cave1
        (recur cave1 x (inc fromy) toy)))))

(defn draw-horizontally [cave y fromx tox]
  (let [cave1 (update-cave cave fromx y :rok)]
    (if (= fromx tox)
      cave1
      (recur cave1 y (inc fromx) tox))))

(defn cave-content [cave x y]
  (x (y cave)))

(defn draw-line [cave [fromx fromy] [tox toy]]
  (cond
    (and (= fromx tox)
         (< fromy toy)) (draw-vertically cave fromx fromy toy)
    (and (= fromx tox)
         (> fromy toy)) (draw-vertically cave fromx toy fromy)
    (and (= fromy toy)
         (< fromx tox)) (draw-horizontally cave fromy fromx tox)
    (and (= fromy toy)
         (> fromx tox)) (draw-horizontally cave fromy tox fromx)))

(defn draw-lines [cave [from to & lines]]
  (let [cave1 (draw-line cave from to)]
    (if (not (seq lines))
      cave1
      (recur cave1 (cons to lines)))))

(defn initialize [cave [lines & more-lines]]
  (if (not (seq lines))
    cave
    (let [cave1 (draw-lines cave lines)]
      (recur cave1 more-lines))))

(defn place-sand [cave x y]
  (if (or (= x 0) (>= (inc y) (count cave)))
    cave
    (let [y1 (inc y)
          front ((cave y1) x)
          left ((cave y1) (dec x))
          right ((cave y1) (inc x))]
      (cond
        (= :rok front left right) (update-cave cave x y :rok)
        (= :air front) (place-sand cave x (inc y))
        (= :air left) (place-sand cave (dec x) y1)
        (= :air right) (place-sand cave (inc x) y1)))))

(defn keep-sand-flowing [cave x y count]
  (let [cave1 (place-sand cave x y)]
    (if (= cave1 cave)
      count
      (recur cave1 x y (inc count)))))

(defn solution-part-2 []
  (let [cave (initialize my-cave input)]
    (keep-sand-flowing cave 500 0 0)))
