(ns lab2.lab-2-1
  (:require [lab2.lab-2-base :refer [integrate-not-optimized]]))

(def definite-integral-faster-but-overflows*
  (memoize (fn [f n step memoize-step]
             (if (<= n 0)
               (integrate-not-optimized f 0 (* n memoize-step) step)
               (+
                 (definite-integral-faster-but-overflows* f (dec n) step memoize-step)
                 (integrate-not-optimized f (* (dec n) memoize-step) (* n memoize-step) step))))))

(def definite-integral*
  (memoize (fn [f x step] (integrate-not-optimized f 0 x step))))


(defn definite-integral
  [f x step memoize-step]
  (cond
    (< step 0) nil
    (== 0 x) 0
    :else (let [n (Math/floor (/ x memoize-step))
                n0 (if (== x (* n memoize-step)) n (dec n))
                x0 (* memoize-step n0)]
            (+ (definite-integral* f x0 step) (integrate-not-optimized f x0 x step)))))

(defn integrator
  [f integrate-step memoize-step]
  (if (or (<= integrate-step 0) (<= memoize-step 0))
    nil
    (fn [x] (definite-integral f x integrate-step memoize-step))))


(defn -main
  []
  (let [f (fn [x] (+ (* 3 x x) (* 4 x) 5))
        step 0.1
        end 1500
        plot-step 0.5]
    ; load classes and whatnot
    (integrate-not-optimized dec 0 0 step)
    (definite-integral dec 100 1 1)

    (let [F (integrator f step (* plot-step 20))
          normal-result (time (doall (map (fn [x] (integrate-not-optimized f 0 x step)) (range 0 end plot-step))))
          mem-result (time (doall (map F (range 0 end plot-step))))]
      ; "Elapsed time: 6893.6761 msecs"
      ; "Elapsed time: 470.3333 msecs"
      (println (last normal-result))
      (println (last mem-result)))))
