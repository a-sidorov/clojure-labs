(ns dnf.operations)

(defn constant
  [value] (list ::const (if value true false)))

(defn constant?
  [expr] (= (first expr) ::const))

(defn variable
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  [expr] (= (first expr) ::var))

(defn leaf?
  "Check if it leaf node (variable or constant)"
  [expr]
  (or
    (variable? expr)
    (constant? expr)))
(defn get-args
  [expr] (rest expr))
(defn conjunction [expr & rest]
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn conjunction? [expr] (= (first expr) ::and))

(defn disjunction [expr & rest]
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn disjunction? [expr] (= (first expr) ::or))

(defn negation [expr] (list ::not expr))

(defn negation? [expr] (= (first expr) ::not))

(defn implication [expr-x expr-y] (list ::impl expr-x expr-y))

(defn implication? [expr] (= (first expr) ::impl))

(defn get-implication-first-arg [impl]
  {:pre (implication? impl)}
  (first (rest impl)))

(defn get-implication-second-arg [impl]
  {:pre (implication? impl)}
  (last (rest impl)))

(defn same-type?
  [expr1 expr2] (= (first expr1) (first expr2)))

(defn update-args [expr new-args]
  (if (> (count new-args) 1)
    (cons (first expr) new-args)
    (list (first expr) (first new-args))))

(defn- collect-args
  [expr]
  (if (leaf? expr)
    (list expr)
    (->> (get-args expr)
         (map (fn [arg] (if (same-type? expr arg)
                          (collect-args arg)
                          (list arg))))
         (apply concat))))
(defn dnf-of-type [expr args] (cons (first expr) args))

(defn decompose
  "(a*(b*c)) -> (a*b*c)"
  [expr]
  (if (and (not (leaf? expr))
           (or
             (disjunction? expr)
             (conjunction? expr))
           )
    (update-args expr (map #(decompose %) (collect-args expr)))
    expr))
(def C-TRUE (constant true))
(def C-FALSE (constant false))