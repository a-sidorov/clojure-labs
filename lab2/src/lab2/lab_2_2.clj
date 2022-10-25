(ns lab2.lab-2-2
  (:require [lab2.lab-2-base :refer [integrate-not-optimized]]))

(defn definite-integral-sequence
  [f integrate-step sequence-step]
  (if (or (<= integrate-step 0) (<= sequence-step 0))
    nil
    (iterate
      (fn [[integral-value n]]
        (let [x0 (* sequence-step n)
              x1 (* sequence-step (inc n))]
          [(+ integral-value (integrate-not-optimized f x0 x1 integrate-step)) (inc n)]))
      [0 0])))

(defn integrator
  [f integrate-step sequence-step]
  (let [integral-sequence (definite-integral-sequence f integrate-step sequence-step)]
    (if (nil? integral-sequence)
      nil
      (fn [x]
        (if (< x 0)
          nil
          (let [n (Math/floor (/ x sequence-step))
                [base-integral-value _] (nth integral-sequence n)
                x0 (* sequence-step n)]
            (+ base-integral-value (integrate-not-optimized f x0 x integrate-step))))))))

(defn -main
  []
  (let [f (fn [x] (+ (* 3 x x) (* 4 x) 5))
        step 0.1
        end 100
        plot-step 0.5]
    ; load classes and whatnot
    (integrate-not-optimized dec 0 0 step)
    (nth (definite-integral-sequence dec step 1) 0)

    (let [F (integrator f step (* plot-step 20))
          normal-result (time (doall (map (fn [x] (integrate-not-optimized f 0 x step)) (range 0 end plot-step))))
          mem-result (time (doall (map F (range 0 end plot-step))))]
      ; "Elapsed time: 17043.0674 msecs"
      ; "Elapsed time: 171.3535 msecs"
      (println (last normal-result))
      (println (last mem-result)))))
