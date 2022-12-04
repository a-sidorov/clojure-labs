(ns lab2.lab-2-2
  (:require [lab2.lab-2-base :refer [integrate-not-optimized integrate-not-optimized-seq]]))

(defn integrator
  [f integrate-step]
  (let [integral-sequence (integrate-not-optimized-seq f 0 integrate-step)]
    (if (nil? integral-sequence)
      nil
      (fn [x]
        (if (< x 0)
          nil
          (let [n (Math/floor (/ x integrate-step))]
            (nth integral-sequence n)))))))

(defn -main
  []
  (let [f (fn [x] (+ (* 3 x x) (* 4 x) 5))
        step 0.5
        end 3000]
    ; load classes and whatnot
    (integrate-not-optimized dec 0 0 step)

    (let [F (integrator f step)
          normal-result (time (doall (map (fn [x] (integrate-not-optimized f 0 x step)) (range 0 end step))))
          mem-result (time (doall (map F (range 0 end step))))]
      ;"Elapsed time: 6712.2166 msecs"
      ;"Elapsed time: 198.1263 msecs"
      (println (last normal-result))
      (println (last mem-result)))))
