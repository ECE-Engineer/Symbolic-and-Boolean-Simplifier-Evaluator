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



;return the symbol in a list if one is there
(defn get-symbol-list [lst]
  (empty? (remove (fn[x] (= 'not x)) (remove (fn[x] (= 'or x)) (remove (fn[x] (= 'and x)) (remove false? (remove true? (flatten lst))))))) nil
  :else (remove (fn[x] (= 'not x)) (remove (fn[x] (= 'or x)) (remove (fn[x] (= 'and x)) (remove false? (remove true? (flatten lst))))))
)

;return the original expression with the values substituted in
(defn build-expression [expr keys-to-map hashmap]
  (cond
    (empty? keys-to-map) nil
    (empty? (rest keys-to-map)) (deep-substitute expr (hash-map (first keys-to-map) (get hashmap (first keys-to-map))))
    :else (build-expression (deep-substitute expr (hash-map (first keys-to-map) (get hashmap (first keys-to-map)))) (rest keys-to-map) hashmap)
  )
)

;make a hashmap for the all the new bindings that need to be made with the default true value
(defn make-default-bindings [left-over-keys]
  (cond
    (empty? left-over-keys) nil
    (empty? (rest left-over-keys)) (hash-map (first left-over-keys) true)
    :else (merge (hash-map (first left-over-keys) true) (make-default-bindings (rest left-over-keys)))
  )
)

;given an expression and a key-value-pair map, bind the values to the keys found in the expr
(defn simplify [expr bindings]
  (def keys-to-map (keys bindings))
  (def new-expr (build-expression expr keys-to-map bindings))
  (def keys-to-map (get-symbol-list new-expr))
  (cond
    (empty? keys-to-map) new-expr
    :else (build-expression new-expr keys-to-map (make-default-bindings keys-to-map))
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (simplify '(and x (or x (and y (not z)))) '{x true, y true}))
;(println (get-symbol-list (simplify '(and x (or x (and y (not z)))) '{x true, y true})))
;(println (simplify '(and x (or x (and y (not z)))) '{x true, y true, z true}))
;(println (eval (simplify '(and x (or x (and y (not z)))) '{x true, y true, z true})))
;(println (eval (simplify '(and x (or x (and y (not z)))) '{x true, y true})))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;evaluate any expression with its repective bindings
(defn evalexp [expr bindings]
  (eval (simplify expr bindings))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(println (get-symbol (simplify '(and x (or x (and y (not z)))) '{x true, y true})));;;;;;;;;;;;;;;;;;doesn't work
;(println (evalexp '(and x (or x (and y (not z)))) '{x true, y true, z true}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEMO
;(def p1 '(and x (or x (and y (not z)))))
;(def p2 '(and (and z false) (or x true)))
;(def p3 '(or true a))
;(println (evalexp p1 '{x false, z true}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;