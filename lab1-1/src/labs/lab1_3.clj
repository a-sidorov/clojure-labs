(ns labs.lab1-3)

(defn my-map [f coll]
  concat
  (reduce (fn [acc elem] (concat acc [(f elem)])) [] coll))

(defn my-filter [pred coll]
  (reduce (fn [acc elem] (if (pred elem) (concat acc [elem]) acc)) [] coll))

(defn -main [& args]
  (time (doall (my-map inc (range 10))))
  (time (doall (my-map inc (range 100))))
  (time (doall (my-map inc (range 1000))))
  (time (doall (my-map inc (range 10000))))


  (time (doall (my-filter even? (range 10))))
  (time (doall (my-filter even? (range 100))))
  (time (doall (my-filter even? (range 1000))))
  (time (doall (my-filter even? (range 10000))))

  (println (my-map inc [1 2 3 4 5]))
  (println (my-map inc (list 1 2 3 4 5)))
  (println (my-map dec [1 2 3 4 5]))
  (println (my-map dec (list 1 2 3 4 5)))
  (println (my-filter even? (range 10)))
  (println (my-filter (fn [x] (>= x 7)) (range 10)))
  )