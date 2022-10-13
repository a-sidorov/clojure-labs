(ns labs.lab1-4)

;Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;Для символов 'а', 'b', 'c' и n=2 результат должен быть
;("ab" "ac" "ba" "bc" "ca" "cb") с точностью до перестановки.

;1.1 Решите задачу с помощью элементарных операций над последовательностями и рекурсии
;1.2. Перепишите программу 1.1. так, чтобы все рекурсивные вызовы были хвостовыми


(defn expand-word [word alphabet]
  (letfn [(filter-first [symb] (not= (first word) symb))]
    (letfn [(add-map [filtered-alphabet] (cons filtered-alphabet word))]
      (map add-map (filter filter-first alphabet)))
    )
  )                                                         ; На выходе получим коллекцию

(defn generate-next-word [words alphabet]
  "Добавляем символ к каждому слову из последовательности"
  (letfn [(expand-word-map [word] (expand-word word alphabet))]
    (apply concat (map expand-word-map words))        ;замена mapcat
    ))

(defn generate-permutations [symbols n]
  (if (< n 0)
    ()
    (if (not= n 0)
      (nth (iterate (fn [acc] (generate-next-word acc symbols)) (list (list))) n)
      (list (list))
      ))
  )

(defn -main
  []
  (println (generate-permutations (list "b" "a" "g") 3))
  (println (generate-permutations (list :a :b :g) 3))
  (println (generate-permutations (list '("b") '("a") '("g")) 3))
  )
