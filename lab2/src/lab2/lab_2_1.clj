(ns lab2.lab-2-1
  (:require [lab2.lab-2-base :refer [integrate-not-optimized trapezoid-area]]))

(def definite-integral*
  (memoize (fn [f x step] (integrate-not-optimized f 0 x step))))

(def definite-integral
  (memoize (fn [f x step] (cond
                            (< step 0) nil
                            (== 0 x) 0
                            :else (+ (definite-integral f (- x step) step) (trapezoid-area (f (- x step)) (f x) step))))))

(defn integrator
  [f integrate-step]
  (if (<= integrate-step 0)
    nil
    (fn [x] (definite-integral f x integrate-step))))


(defn -main
  []
  (let [f (fn [x] (+ (* 3 x x) (* 4 x) 5))
        step 0.5
        end 3000]
    ; load classes and whatnot
    (integrate-not-optimized dec 0 0 step)
    (definite-integral dec 100 1)

    (let [F (integrator f step)
          normal-result (time (doall (map (fn [x] (integrate-not-optimized f 0 x step)) (range 0 end step))))
          mem-result (time (doall (map F (range 0 end step))))]
      ;"Elapsed time: 6866.0501 msecs"
      ;"Elapsed time: 17.7733 msecs"
      (println (last normal-result))
      (println (last mem-result)))))
