(ns dnf.dnf-transformer ( :use dnf.operations dnf.io))

(defn apply-recur
  [f expr]
  (let [new-expr (f expr)]
    (cond
      (nil? new-expr) nil
      (or (variable? new-expr) (constant? new-expr)) new-expr
      :else (update-args new-expr (map #(apply-recur f %) (get-args new-expr))))))

(defn transform-implication
  [expr]
  (if (implication? expr)
    (disjunction
      (negation (get-implication-first-arg expr))
      (get-implication-second-arg expr))
    expr))

(defn open-brackets
  [expr]
  (if (negation? expr)
    (let [arg (first (get-args expr))]
      (cond
        (negation? arg) (first (get-args arg))
        (constant? arg) (if (= arg C-TRUE) C-FALSE C-TRUE)
        (conjunction? arg) (apply disjunction (map (fn [x] (negation x)) (get-args arg)))
        (disjunction? arg) (apply conjunction (map (fn [x] (negation x)) (get-args arg)))
        :else expr))
    expr))

(defn- dist-conjunction-helper
  [and-arg or-expr]
  (disjunction (conjunction and-arg (first (get-args or-expr))) (conjunction and-arg (last (get-args or-expr)))))

(defn distributive-conjunction
  "a*(b+c) = a*b+a*c"
  [expr]
  (if (conjunction? expr)
    (let [a (first (get-args expr)) b (last (get-args expr))]
      (cond
        (disjunction? a) (dist-conjunction-helper b a)
        (disjunction? b) (dist-conjunction-helper a b)
        :else expr))
    expr))

(defn- args-contains-const?
  [expr const]
  (some #(and (constant? %)
              (= const %))
        (get-args expr)))
(defn simplify
  "Simplify conjunction and disjunction if there constant"
  [expr]
  (let [expr (decompose expr)]
    (if (leaf? expr)
      expr
      (cond
        (and (conjunction? expr)
             (args-contains-const? expr C-FALSE)) C-FALSE
        (and (conjunction? expr)
             (args-contains-const? expr C-TRUE)) (apply conjunction (filter #(not (= % C-TRUE)) (get-args expr)))
        (and (disjunction? expr)
             (args-contains-const? expr C-TRUE)) C-TRUE
        (and (disjunction? expr)
             (args-contains-const? expr C-FALSE)) (apply disjunction (filter #(not (= % C-FALSE)) (get-args expr)))
        :else expr)
      )))
(defn to-dnf [expr]
  (->> expr
       (apply-recur transform-implication)
       (apply-recur open-brackets)
       (apply-recur distributive-conjunction)
       (apply-recur simplify)))

(defn substitute
  [expr var-map]
  (apply-recur simplify
               (cond
                 (variable? expr) (let [key (first (get-args expr))]
                                    (if (contains? var-map key)
                                      (constant (get var-map key))
                                      expr))
                 (constant? expr) expr
                 :else (dnf-of-type expr (map (fn [expr] (substitute expr var-map)) (get-args expr))))))
