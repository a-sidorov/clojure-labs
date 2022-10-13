(ns labs.lab1-3)

(defn my-map [f coll]
  (reduce (fn [acc elem] (conj acc (f elem))) [] coll))

(defn my-filter [pred coll]
  (reduce (fn [acc elem] (if (pred elem) (conj acc elem) acc)) [] coll))

(defn -main [& args]
  (println (my-map inc [1 2 3 4 5]))
  (println (my-map dec [1 2 3 4 5]))
  (println (my-filter even? (range 10)))
  (println (my-filter (fn [x] (>= x 7)) (range 10)))
  )