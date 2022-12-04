(ns lab2.lab-2-base)


(defn trapezoid-area
  [a b h]
  (/ (* h (+ a b)) 2))


(defn integrate-not-optimized-old
  "Compute integral of f from 'start' to 'end' with step 'step'
  Step must be positive"
  [f start end step]
  (cond
    (<= step 0.0) nil
    (== end start) 0.0
    (< end start) (- (integrate-not-optimized-old f end start step))
    :else (let [xs (range start end step)
                ys (map f xs)
                f-values (map list ys (rest ys))
                rest (- end (last xs))]
            (reduce
              (fn [result [f0 f1]] (+ result (trapezoid-area f0 f1 step)))
              (trapezoid-area (f end) (f (- end rest)) rest)
              f-values))))

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
                f-values (map list ys (rest ys))]
            (letfn [(area [[f0 f1]] (trapezoid-area f0 f1 step))]
              (last (reductions + (map area f-values)))))))

(defn integrate-not-optimized-seq
  "Compute integral of f from 'start' to 'end' with step 'step'
  Step must be positive"
  [f start step]
  (cond
    (<= step 0.0) nil
    :else (let [xs (iterate (fn [x0] (+ x0 step)) start)
                ys (map f xs)
                f-values (map list ys (rest ys))]
            (letfn [(area [[f0 f1]] (trapezoid-area f0 f1 step))]
              (reductions + (map area f-values))))))
