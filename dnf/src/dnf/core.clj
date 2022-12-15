(ns dnf.core
  (:use dnf.io
        dnf.dnf-transformer))

(defn -main []
  (println (to-dnf (parse "(a+b)>(1+d>x)")))
  (println (substitute (to-dnf (parse "(a+b)>(c+d>x)")) {:a 1 :b 0 :c 1 :d 1 :x 0}))
  )
