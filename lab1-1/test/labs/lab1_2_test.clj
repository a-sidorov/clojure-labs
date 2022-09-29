(ns labs.lab1_2-test
  (:require [clojure.test :refer [deftest is testing]],
            [labs.lab1-2 :refer [expand-word
                                   generate-next-word
                                   generate-permutations
                                   ]]))

(deftest expand-word-test
  (testing "Empty collections list"
    (is (=
          (expand-word [:a], `())
          [])))
  (testing "Non-empty collections list"
    (is (=
          (expand-word `("b" "c") `("a" "d"))
          [["d", "b", "c"], ["a", "b", "c"]])
        )
    )
  )

(deftest generate-next-word-test
  (testing "Empty collections list"
    (is (=
          (generate-next-word [:a], `())
          [])))
  (testing "Non-empty collections list"
    (is (=
          (generate-next-word `(("b") ("c")) `("a" "d"))
          [["d", "b"], ["a", "b"], ["d", "c"], ["a", "c"]]
          ))
    )
  )

(deftest generate-permutations-test
  (testing "Test permutations"
    (is (=
          (generate-permutations `("b" "a") 3)
          [["a", "b", "a"], ["b", "a", "b"]]
          )))
  (testing "Test permutations vars"
    (is (=
          (generate-permutations `(:b :a) 3)
          [[:a, :b, :a], [:b, :a, :b]]
          )))
  )
