(ns r-horizon.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clj-time.core :as time]
            [clj-time.periodic :as time-period]
            [clj-time.format :as time-fmt]))

(import '(java.util Random))



(defn time-range
  "Return a lazy sequence of DateTime's from start to end, incremented
  by 'step' units of time."
  [start end step]
  (let [inf-range (time-period/periodic-seq start step)
        below-end? (fn [t] (time/within? (time/interval start end)
                                         t))]
    (take-while below-end? inf-range)))

(defn map_rng [[a1 a2] [b1 b2] s]
  (+ b1 (/ (* (- s a1) (- b2 b1)) (- a2 a1))))

(def abs_rng_val 1300)  ;; - abs to + abs_rng, by step size
(def step_size 0.015)   ;;
(def x_len (int (Math/ceil (/ (* 2 abs_rng_val) step_size)))) 
;;(def x_len 7000)  ;;Math.ceil(173334 / 6755);

;;(def x (take x_len (iterate inc (- abs_rng_val))))
(def x (range (- abs_rng_val) abs_rng_val step_size))

(def ins_ (time-range (time/date-time 2001 01 01)
            (time/date-time 2019 07 01)
            (time/days 1)))

(def xx_ (time-range (time/date-time 2003 01 01)
                      (time/date-time 2018 07 01)
                      (time/days 1)))

(def ins (take x_len (cycle ins_)))
(def xx (take x_len (cycle xx_)))

;;(time-fmt/show-formatters)
(def fmt-dm (time-fmt/formatter "ddMM"))
(def fmt-Ym (time-fmt/formatter "yyyyMM"))
(def fmt-y (time-fmt/formatter "y"))

;;a <- as.numeric (format (ins, "%d%m"))
(def a (map #(Integer/parseInt (time-fmt/unparse fmt-dm %1)) ins))
;;b <- as.numeric(format(ins, "%Y%m"))
(def b (map #(Integer/parseInt (time-fmt/unparse fmt-Ym %1)) ins))
;;c <- as.numeric(format(xx, "%d%m"))
(def c (map #(Integer/parseInt (time-fmt/unparse fmt-dm %1)) xx))
;;d <- as.numeric(format(xx, "%y"))
(def d (map #(Integer/parseInt (subs (time-fmt/unparse fmt-y %1) 2)) xx))


(defn next-g [n mean sd]
  (let [r (Random.)]
    (repeatedly n #(-> r .nextGaussian (* sd) (+ mean)))))

;;

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 1 1 1)
  (q/background 220 0.02 0.99)
  ; setup function returns initial state. 
  {:c (map #(Integer/parseInt (time-fmt/unparse fmt-dm %1)) ins)
   :d (map #(Integer/parseInt (time-fmt/unparse fmt-Ym %1)) ins)
   :e (map #(Integer/parseInt (time-fmt/unparse fmt-Ym %1)) ins)})

(defn update-state [state]
  (let
   [r (Random.)
    c (map #(Integer/parseInt (time-fmt/unparse fmt-dm %1)) ins)
    rnd1512 (next-g (count c) 15.0 12.0)
    d (map #(Integer/parseInt (time-fmt/unparse fmt-Ym %1)) ins)
    rnd1505 (next-g (count d) 15.0 35.0)
    e (map #(Integer/parseInt (time-fmt/unparse fmt-Ym %1)) ins)
    rnd0506 (next-g (count e) 5.0 6.0 )]
    {:c (mapv + c rnd1512)
     :d (mapv + d rnd1505)
     :e (mapv + e rnd0506)}))

(defn draw-state [state]
  (if (q/key-pressed?)
    (do
      (println "saving")
      (q/save-frame "C:/Users/Jason/Documents/Art Output/r_horizon-####.tiff")))
  
  (let [xs (map #(Math/cos (/ %1 %2)) (:e state) x)
        ys (map #(Math/sin (/ %1 (* %2 %3))) (:d state) x (:c state))
        fills (map #(map_rng [-1 1] [212 108] %1) ys)
        alphs (map #(map_rng [-1 1] [0.05 0.15] %1) ys)
        ;fill (map_rng (p [1], -1, 1, 90, 320), 58, 90, .25)
        xs (map #(* (/ (q/width) 2) %1) xs)
        ys (map #(* (/ (q/height) 2) %1) ys)
        pts (map vector xs ys fills alphs)]
    ;;(println (take 10 xs) (take 10 ys))
    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      ;;(q/fill 100 0.5 0.7 0.2)
      (q/no-stroke)
      ;;(println "inside" (first xs) (first ys))
      (doseq [pt pts]             
        ;;(println (nth pt 2))
        (let [new-pt [(first pt) (second pt)]
              fill-col (nth pt 2)
              alph-col (nth pt 3)]
          (q/fill fill-col 0.54 0.9 alph-col) ;;fill)
          ;; TRY SIZE BEING 1 31 instead of 1 1 
          (apply #(q/ellipse %1 %2 1 1) new-pt)))
      ))
  )

(q/defsketch r-horizon
  :title "R Horizons"
  :size [1600 1600]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
