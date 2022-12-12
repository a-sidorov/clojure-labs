(ns lab3.utils)

(def test-coll (iterate inc 0))

(defn heavy-even? [x] (do (Thread/sleep 100) (even? x)))