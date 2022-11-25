(ns lab3.lab3-1 (:use lab3.utils))

(defn cut-sequence [parts-count sequence]
  "Порезать последовательность на части"
  (take-while #(not (empty? %))
              (lazy-seq
                (cons
                  (take parts-count sequence)
                  (cut-sequence parts-count (drop parts-count sequence))))))

(defn parallel-filter [predicate thread-count sequence]
  (->> sequence
       (cut-sequence thread-count)
       (map #(future (doall (filter predicate %))))
       (doall)
       (map deref)
       (apply concat)))

(defn -main []
  (let [
        test-seq (take 1000 test-coll)
        ]
    (time (doall (filter heavy-even? test-seq)))            ;"Elapsed time: 110761.4145 msecs"
    (time (doall (parallel-filter heavy-even? 4 test-seq))) ;"Elapsed time: 540.2831 msecs"
    (time (doall (parallel-filter heavy-even? 8 test-seq))) ;"Elapsed time: 884.5593 msecs"
    )
  )


