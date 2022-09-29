(ns lab1-1.lab1_1-test
  (:require [clojure.test :refer [deftest is testing]],
            [lab1-1.lab1-1 :refer [expand-word
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
          [["a", "b", "c"], ["d", "b", "c"]])
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
          [["a", "b"], ["d", "b"], ["a", "c"], ["d", "c"]]
          ))
    )
  )

(deftest generate-permutations-test
  (testing "Test permutations"
    (is (=
          (generate-permutations `("b" "a") 3)
          [["b", "a", "b"], ["a", "b", "a"]]
          )))
  (testing "Test permutations vars"
    (is (=
          (generate-permutations `(:b :a) 3)
          [[:b, :a, :b], [:a, :b, :a]]
          )))
  )
