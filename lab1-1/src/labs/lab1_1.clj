(ns labs.lab1-1)

;Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;Для символов 'а', 'b', 'c' и n=2 результат должен быть
;("ab" "ac" "ba" "bc" "ca" "cb") с точностью до перестановки.

;lab 1.1 Решите задачу с помощью элементарных операций над последовательностями и рекурсии

(defn expand-word [word alphabet]
  "Добавляет в начало слова символ из алфавита"
  (if (empty? alphabet)
    ()                                                      ; Алфавит кончился и мы тоже
    (if (not= (first word) (first alphabet))
      (cons (cons (first alphabet) word) (expand-word word (rest alphabet))) ; Символы не повторяются берем следующий символ из набора и целяем к слову. Дальше рекурсивно с остатком набора
      (expand-word word (rest alphabet))                    ; Символы не должны повторяться, просто пропускаем
      )
    )
  )                                                         ; На выходе получим коллекцию
;  (add-symbol (list  "a" "g") (list "a" "b" "d" "e")) => ((b a g) (d a g) (e a g))



(defn generate-next-word [words alphabet]
  "Добавляем символ к каждому слову из последовательности"
  (if (empty? words)
    ()
    (concat (expand-word (first words) alphabet) (generate-next-word (rest words) alphabet))
    ))


(defn my-loop [words alphabet n]
  "цикл"
  (if (= n 0)
    words
    (my-loop (generate-next-word words alphabet) alphabet (dec n))))

(defn generate-permutations [symbols n]
  (if (< n 0)
    ()
    (my-loop (list (list)) symbols n)))

(defn -main
  []
  (println (generate-permutations (list "b" "a" "g") -3))
  (println (generate-permutations (list "b" "a" "g") 0))
  (println (generate-permutations (list "b" "a" "g") 3))
  (println (generate-permutations (list "b" "a" "g") 1))
  (println (generate-permutations (list :a :b :g) 3))
  (println (generate-permutations (list '("b") '("a") '("g")) 3))
  )
