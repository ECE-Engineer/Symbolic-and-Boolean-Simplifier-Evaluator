(ns Main)

;generates a list containing n number of lst items
(defn generate [n lst]
 (cond
   (= n 0) ()
   :else  (cons lst (generate (- n 1) lst))
 )
)

;a general purpose method to construct an expression using a logical operator and other expressions
(defn createExpr [act lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) (first lst)
    :else (list act (first lst) (createExpr act (rest lst)))
  )
)

;create an expression using a logical and operator and other expressions
(defn andexp [& rest]
  (createExpr 'and rest)
)

;create an expression using a logical or operator and other expressions
(defn orexp [& rest]
  (createExpr 'or rest)
)

;create an expression using a logical not operator and an expression
(defn notexp [e1] (list 'not e1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (orexp 'a))
;(println (orexp true 'a))
;(println (orexp true 'a false))
;(println (andexp 'a))
;(println (andexp true 'a))
;(println (andexp true 'a false))
;(println (notexp 'a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;substitute all values to a key for a given expression
(defn deep-substitute [expr key-value-pair]
  (map #(cond
          (seq? %) (deep-substitute % key-value-pair)
          :default (key-value-pair % %))
       expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (deep-substitute '(and x (or x (and y (not z)))) '{x 4}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;return the original expression with the values substituted in
(defn build-expression [expr keys-to-map hashmap]
  (cond
    (empty? keys-to-map) nil
    (empty? (rest keys-to-map)) (deep-substitute expr (hash-map (first keys-to-map) (get hashmap (first keys-to-map))))
    :else (build-expression (deep-substitute expr (hash-map (first keys-to-map) (get hashmap (first keys-to-map)))) (rest keys-to-map) hashmap)
  )
)

;substitue a predicate in an expression with the the replacement
(defn substitute [expr key-value-pair]
  (map #(key-value-pair % %) expr))

;get's the first list in the expression
(defn get-inner-list [expr]
  (cond
    (empty? expr) nil
    (and (empty? (rest expr)) (list? (first expr))) (first expr)
    :else (get-inner-list (rest expr))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (get-list '(and false (or true))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace-pred [expr pred repl];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    (some (fn [x] (= pred x)) (list expr)) (substitute (list expr) (hash-map pred repl))
    (some (fn [x] (= pred x)) expr) (substitute expr (hash-map pred repl))
    :else expr
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (replace-pred  '(and false (or true)) '(or true) 'true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;return the original expression with the values substituted in
(defn build-replace-expression [expr pred repl]
  (cond
    (empty? expr) nil
    (or (some (fn [x] (= pred x)) (list expr)) (some (fn [x] (= pred x)) expr)) (replace-pred expr pred repl)
    :else (cond
            (= (count expr) 3) (list (nth expr 0) (nth expr 1) (build-replace-expression (get-inner-list expr) pred repl))
            :else (list (nth expr 0) (build-replace-expression (get-inner-list expr) pred repl))
          )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (build-replace-expression '(and false (and false (or true))) '(or true) 'true))
;(println (build-replace-expression '(and false (and false (not false))) '(not false) 'true))
;(println (build-replace-expression '(not (not false)) '(not false) 'true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;returns whether or not the predicate was found in the expression
(defn pred-present [expr pred]
  (cond
    (or (some (fn [x] (= pred x)) (list expr)) (some (fn [x] (= pred x)) expr)) true
    :else false
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (pred-present '(and false (or true)) '(or true)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;search all the levels of an expression to try an find a matching specific expression : parameters are a predicate and an expression
(defn deep-search [expr pred];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    (empty? expr) nil
    (or (some (fn [x] (= pred x)) (list expr)) (some (fn [x] (= pred x)) expr)) (pred-present expr pred)
    :else (deep-search (get-inner-list expr) pred)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (deep-search '(and false (and false (or true))) '(or true)))
;(println (deep-search '(and false (and false (not false))) '(not false)))
;(println (deep-search '(not (not false)) '(not false)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;make a hashmap for the all the new bindings that need to be made with the default true value
(defn make-default-bindings [left-over-keys]
  (cond
    (empty? left-over-keys) nil
    (empty? (rest left-over-keys)) (hash-map (first left-over-keys) true)
    :else (merge (hash-map (first left-over-keys) true) (make-default-bindings (rest left-over-keys)))
  )
)

;return the symbol in a list if one is there
(defn get-symbol-list [lst]
  (empty? (remove (fn[x] (= 'not x)) (remove (fn[x] (= 'or x)) (remove (fn[x] (= 'and x)) (remove false? (remove true? (flatten lst))))))) nil
  :else (remove (fn[x] (= 'not x)) (remove (fn[x] (= 'or x)) (remove (fn[x] (= 'and x)) (remove false? (remove true? (flatten lst))))))
)

;make a table with conditions to check for when simplifying the expressions
(defn property-simplification [expr];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    (deep-search expr '(or true)) (property-simplification (build-replace-expression expr '(or true) 'true))
    (deep-search expr '(or false)) (property-simplification (build-replace-expression expr '(or false) 'false))
    (deep-search expr '(and true)) (property-simplification (build-replace-expression expr '(and true) 'true))
    (deep-search expr '(and false)) (property-simplification (build-replace-expression expr '(and false) 'false))
    (deep-search expr (list 'or 'x 'false)) (property-simplification (build-replace-expression expr (list 'or 'x 'false) 'x))
    (deep-search expr (list 'or 'false 'x)) (property-simplification (build-replace-expression expr (list 'or 'false 'x) 'x))
    (deep-search expr (list 'or 'true 'x)) (property-simplification (build-replace-expression expr (list 'or 'true 'x) 'true))
    (deep-search expr (list 'or 'x 'true)) (property-simplification (build-replace-expression expr (list 'or 'x 'true) 'true))
    (deep-search expr (list 'and 'x 'false)) (property-simplification (build-replace-expression expr (list 'and 'x 'false) 'false))
    (deep-search expr (list 'and 'false 'x)) (property-simplification (build-replace-expression expr (list 'and 'false 'x) 'false))
    (deep-search expr (list 'and 'x 'true)) (property-simplification (build-replace-expression expr (list 'and 'x 'true) 'x))
    (deep-search expr (list 'and 'true 'x)) (property-simplification (build-replace-expression expr (list 'and 'true 'x) 'x))
    (deep-search expr (list 'or 'y 'false)) (property-simplification (build-replace-expression expr (list 'or 'y 'false) 'y))
    (deep-search expr (list 'or 'false 'y)) (property-simplification (build-replace-expression expr (list 'or 'false 'y) 'y))
    (deep-search expr (list 'or 'true 'y)) (property-simplification (build-replace-expression expr (list 'or 'true 'y) 'true))
    (deep-search expr (list 'or 'y 'true)) (property-simplification (build-replace-expression expr (list 'or 'y 'true) 'true))
    (deep-search expr (list 'and 'y 'false)) (property-simplification (build-replace-expression expr (list 'and 'y 'false) 'false))
    (deep-search expr (list 'and 'false 'y)) (property-simplification (build-replace-expression expr (list 'and 'false 'y) 'false))
    (deep-search expr (list 'and 'y 'true)) (property-simplification (build-replace-expression expr (list 'and 'y 'true) 'y))
    (deep-search expr (list 'and 'true 'y)) (property-simplification (build-replace-expression expr (list 'and 'true 'y) 'y))
    (deep-search expr (list 'or 'z 'false)) (property-simplification (build-replace-expression expr (list 'or 'z 'false) 'z))
    (deep-search expr (list 'or 'false 'z)) (property-simplification (build-replace-expression expr (list 'or 'false 'z) 'z))
    (deep-search expr (list 'or 'true 'z)) (property-simplification (build-replace-expression expr (list 'or 'true 'z) 'true))
    (deep-search expr (list 'or 'z 'true)) (property-simplification (build-replace-expression expr (list 'or 'z 'true) 'true))
    (deep-search expr (list 'and 'z 'false)) (property-simplification (build-replace-expression expr (list 'and 'z 'false) 'false))
    (deep-search expr (list 'and 'false 'z)) (property-simplification (build-replace-expression expr (list 'and 'false 'z) 'false))
    (deep-search expr (list 'and 'z 'true)) (property-simplification (build-replace-expression expr (list 'and 'z 'true) 'z))
    (deep-search expr (list 'and 'true 'z)) (property-simplification (build-replace-expression expr (list 'and 'true 'z) 'z))
    (deep-search expr (list 'not 'false)) (property-simplification (build-replace-expression expr '(not false) 'true));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (deep-search expr (list 'not 'true)) (property-simplification (build-replace-expression expr '(not true) 'false));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (deep-search expr (list 'or 'x 'y 'z)) (property-simplification (build-replace-expression expr (list 'or 'x 'y 'z) (list 'or 'x ('or 'y 'z))))
    (deep-search expr (list 'and 'x 'y 'z)) (property-simplification (build-replace-expression expr (list 'and 'x 'y 'z) (list 'and 'x ('and 'y 'z))))
    (deep-search expr (list 'not (list 'and 'x 'y))) (property-simplification (build-replace-expression expr (list 'not (list 'and 'x 'y)) (list 'or (list 'not 'x) (list 'not 'y))))
    (deep-search expr (list 'not (list 'or 'x 'y))) (property-simplification (build-replace-expression expr (list 'not (list 'or 'x 'y)) (list 'and (list 'not 'x) (list 'not 'y))))
    (deep-search expr (list 'not (list 'and 'x 'z))) (property-simplification (build-replace-expression expr (list 'not (list 'and 'x 'z)) (list 'or (list 'not 'x) (list 'not 'z))))
    (deep-search expr (list 'not (list 'or 'x 'z))) (property-simplification (build-replace-expression expr (list 'not (list 'or 'x 'z)) (list 'and (list 'not 'x) (list 'not 'z))))
    (deep-search expr (list 'not (list 'and 'y 'z))) (property-simplification (build-replace-expression expr (list 'not (list 'and 'y 'z)) (list 'or (list 'not 'y) (list 'not 'z))))
    (deep-search expr (list 'not (list 'or 'y 'z))) (property-simplification (build-replace-expression expr (list 'not (list 'or 'y 'z)) (list 'and (list 'not 'y) (list 'not 'z))))
    :else expr
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (property-simplification '(and (or true) false)))
;(println (property-simplification '(and false (and false (or true)))))
;(println (property-simplification '(and true false)))
;(println (property-simplification '(not false)));;;;;;;;;;;;;;;;;;;ADDITIONAL CHECKS NEED TO BE MADE FOR EXPRESSIONS WHERE THERE ARE ONLY 2 ARGUMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;given an expression and a key-value-pair map, bind the values to the keys found in the expr
(defn simplify [expr bindings]
  (def keys-to-map (keys bindings))
  (def new-expr (build-expression expr keys-to-map bindings))
  (def keys-to-map (get-symbol-list new-expr))
  (cond
    (empty? keys-to-map) (eval new-expr)
    :else (do
            (def new-expr (property-simplification new-expr))
            (def keys-to-map (get-symbol-list new-expr))
            (cond
              (empty? keys-to-map) (eval new-expr)
              (= (count new-expr) 1) (first new-expr)
              :else new-expr
            )
          )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (simplify '(and x (or x (and y (not z)))) '{x true, y true}))
;(println (simplify '(and x (or x (and y (not z)))) '{x true, y true, z true}))

(println (deep-search '(and x (or x (and y (not true)))) '(not true)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;so it does find it, but it isn't replacing it--------------I THINK IT HAS TO DO WITH THE RECURSION in the simplify method

(println (simplify '(and x (or x (and y (not z)))) '{x false, z true}));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;not quite working yet!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;evaluate any expression with its repective bindings
(defn evalexp [expr bindings]
  (simplify expr bindings)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (evalexp '(and x (or x (and y (not z)))) '{x true, y true, z true}))
;(println (evalexp '(and x (or x (and y (not z)))) '{x true, y true}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(def p1 '(and x (or x (and y (not z)))))
;(def p2 '(and (and z false) (or x true)))
;(def p3 '(or true a))
;(println (evalexp p1 '{x false, z true}));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;not quite working yet!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;