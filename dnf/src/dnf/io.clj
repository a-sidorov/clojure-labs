(ns dnf.io (:use dnf.operations))
(defn encoder [string]
  (map #(cond
          (= % \() ::open
          (= % \)) ::close
          (= % \1) ::true
          (= % \0) ::false
          (= % \*) ::and
          (= % \+) ::or
          (= % \>) ::impl
          (= % \-) ::not
          :else (list ::symbol (keyword (str %)))) string))
(defn split-in [pred? stack]
  [(take-while pred? stack) (drop-while pred? stack)])

(declare reverse-polish-notation)

(defn- concat-to-stack [pred tokens res stack]
  (let [[pop r] (split-in pred stack)]
    (reverse-polish-notation
      (rest tokens)
      (if (empty? pop) res (vec (concat res pop)))
      (cons (first tokens) r))))

(defn reverse-polish-notation [tokens res stack]
  (let [token (first tokens)]
    (cond
      (= token ::true) (reverse-polish-notation (rest tokens) (conj res token) stack)
      (= token ::false) (reverse-polish-notation (rest tokens) (conj res token) stack)
      (= token ::not) (reverse-polish-notation (rest tokens) res (cons token stack))
      (= token ::open) (reverse-polish-notation (rest tokens) res (cons token stack))
      (= token ::close) (let [[pop r] (split-in #(not (= % ::open)) stack)]
                          (reverse-polish-notation
                            (rest tokens)
                            (if (empty? pop) res (vec (concat res pop)))
                            (rest r)))
      (= token ::or) (concat-to-stack #(or (= % ::not) (= % ::and) (= % ::or)) tokens res stack)
      (= token ::and) (concat-to-stack #(or (= % ::not) (= % ::and)) tokens res stack)
      (= token ::impl) (concat-to-stack #(or (= % ::not) (= % ::and)) tokens res stack)
      (= (first token) ::symbol) (reverse-polish-notation (rest tokens) (conj res token) stack)
      :else (if (empty? stack) res (concat res stack)))))

(defn parse
  ([string] (parse (reverse-polish-notation (encoder string) [] `()) `()))
  ([rpn stack] (let [token (first rpn)
                     r (rest rpn)]
                 (cond
                   (= token ::true) (parse r (cons (constant true) stack))
                   (= token ::false) (parse r (cons (constant false) stack))
                   (= token ::not) (parse r (cons (negation (first stack)) (rest stack)))
                   (= token ::or) (parse r (cons (disjunction (second stack) (first stack)) (drop 2 stack)))
                   (= token ::and) (parse r (cons (conjunction (second stack) (first stack)) (drop 2 stack)))
                   (= token ::impl) (parse r (cons (implication (second stack) (first stack)) (drop 2 stack)))
                   (= (first token) ::symbol) (parse r (cons (variable (second token)) stack))
                   :else (first stack)))))
