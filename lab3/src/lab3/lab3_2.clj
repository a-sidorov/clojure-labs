(ns lab3.lab3-2 (:use lab3.utils))

(defn lazy-parallel-filter [predicate sublist-size thread-count sequence]
  (->> (partition (* sublist-size thread-count) sequence)
       (map (fn [list] (->> (partition sublist-size list)
                            (map (fn [x] (future (doall (filter predicate x)))))
                            (doall)
                            (map deref))))
       (flatten)))

(defn -main []
  (let [
        test-seq (take 1000 test-coll)
        ]
    (time (doall (filter heavy-even? test-seq)))            ;"Elapsed time: 109725.9133 msecs"
    (time (doall (lazy-parallel-filter heavy-even? 100 4 test-seq))) ;"Elapsed time: 10966.4377 msecs"
    (time (doall (lazy-parallel-filter heavy-even? 100 8 test-seq))) ;"Elapsed time: 10981.739 msecs" - тут прироста не будет размер части почти размер последовательности
    )
  )
