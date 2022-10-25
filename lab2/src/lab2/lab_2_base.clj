(ns lab2.lab-2-base)


(defn trapezoid-area
  [a b h]
  (* 0.5 h (+ a b)))


(defn integrate-not-optimized
  "Compute integral of f from 'start' to 'end' with step 'step'
  Step must be positive"
  [f start end step]
  (cond
    (<= step 0.0) nil
    (== end start) 0.0
    (< end start) (- (integrate-not-optimized f end start step))
    :else (let [xs (range start end step)
                ys (map f xs)
                f-values (map list ys (rest ys))
                rest (- end (last xs))]
            (reduce
              (fn [result [f0 f1]] (+ result (trapezoid-area f0 f1 step)))
              (trapezoid-area (f end) (f (- end rest)) rest)
              f-values))))
