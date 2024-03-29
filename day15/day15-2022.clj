(ns day15-2022)

(def input '((3859432,2304903,3677247,3140958),
            (2488890,2695345,1934788,2667279),
            (3901948,701878,4095477,368031),
            (2422190,1775708,1765036,2000000),
            (2703846,3282799,2121069,3230302),
            (172003,2579074,-77667,3197309),
            (1813149,1311283,1765036,2000000),
            (1704453,2468117,1934788,2667279),
            (1927725,2976002,1934788,2667279),
            (3176646,1254463,2946873,2167634),
            (2149510,3722117,2121069,3230302),
            (3804434,251015,4095477,368031),
            (2613561,3932220,2121069,3230302),
            (3997794,3291220,3677247,3140958),
            (98328,3675176,-77667,3197309),
            (2006541,2259601,1934788,2667279),
            (663904,122919,1618552,-433244),
            (1116472,3349728,2121069,3230302),
            (2810797,2300748,2946873,2167634),
            (1760767,2024355,1765036,2000000),
            (3098487,2529092,2946873,2167634),
            (1716839,634872,1618552,-433244),
            (9323,979154,-245599,778791),
            (1737623,2032367,1765036,2000000),
            (26695,3049071,-77667,3197309),
            (3691492,3766350,3677247,3140958),
            (730556,1657010,1765036,2000000),
            (506169,3958647,-77667,3197309),
            (2728744,23398,1618552,-433244),
            (3215227,3077078,3677247,3140958),
            (2209379,3030851,2121069,3230302)))

(def sample-input '((2, 18,-2, 15),
                    (9, 16,10, 16),
                    (13, 2,15, 3),
                    (12, 14,10, 16),
                    (10, 20,10, 16),
                    (14, 17,10, 16),
                    (8, 7,2, 10),
                    (2, 0,2, 10),
                    (0, 11,2, 10),
                    (20, 14,25, 17),
                    (17, 20,21, 22),
                    (16, 7,15, 3),
                    (14, 3,15, 3),
                    (20, 1,15, 3)))

(defn abs [x] (if (> x 0) x (- x)))

(defn distance [[a b c d]]
  (+ (abs (- c a)) (abs (- d b))))

(defn is-near [x y [a b c d]]
  (let [d1 (distance [a b c d])
        d2 (distance [a b x y])]
;    (println d1 d2 x y a b c d)
    (and (or (not= x c) (not= y d))
         (<= d2 d1))))

(defn is-near-any [x y [s & sensors]]
  (cond
    (is-near x y s) true
    (empty? sensors) false
    :else (is-near-any x y sensors)))

(defn accumulator [acc p]
  (if (is-near-any p 2000000 input)
    (inc acc)
    acc))

(defn solution1 [input]
  (let [left (reduce min (map - (map distance input) (map first input)))
        right (reduce max (map + (map distance input) (map first input)))]
    (reduce accumulator 0 (range left right))))

(defn calc-y [[a b c d] x]
  (let [d (distance [a b c d])
        a-x (abs (- a x))
        left (- (+ a-x b) d)
        right (- (+ d b) a-x)]
    [left right]))

(defn union [[a b] [c d]]
  (if (>= 1 (- c b))
    [a (max b d)]
    [a b c d]))

(defn union-many [[a b & c]]
;  (println a b c)
  (if (nil? c)
    (union a b)
    (let [r (union a b)]
      (if (= 2 (count r))
        (union-many (cons r c))
        r))))

(defn solution2 []
  (let [abc (map union-many
                 (for [x (range 0 4000000)]
                   (->> input
                        (map #(calc-y % x))
                        (filter #(<= (first %) (last %)))
                        sort)))
        b (drop-while #(= 2 (count %)) abc)
        c (take-while #(= 2 (count %)) abc)
        d (inc (second b))]
    [(count c) d]))
