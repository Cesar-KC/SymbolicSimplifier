#lang racket

;------------PHASE1: FOLD CONSTANTS FUNCTION------------------

(define (fold-constants expr)          
  (cond                                

    ;base cases, if number or symbol only, then return
    [(number? expr) expr]              
    [(symbol? expr) expr]              

    ;if expression is a list
    [(list? expr)                      

     (let* ([operator (first expr)]          ; reads and creates operator as first input of list

            ;; below we recursively call our 'fold-constants' function on the second and third inputs of list
            [left                         
             (fold-constants (second expr))]
            [right                    
             (fold-constants (third expr))])

       ;; after recursion, decide whether we can evaluate(meaning we have only numbers)
       (cond
         ;; if lef/right sides are both numbers, we can compute the correct statement depending on operator.
         [(and (number? left) (number? right))

          (cond
            [(eq? operator '+) (+ left right)]
            [(eq? operator '*) (* left right)]
            [(eq? operator '-) (- left right)]
            [(and (eq? operator '/) (not (= right 0))) (/ left right)] ; this is saying IF operator = '/' AND right symbol != 0, THEN divide! avoid Zero Dvision Error

            ; operator is unknown, it is not (add, subtract, multiply, or divide) so we cant fold
            [else (list operator left right)])]

         ;; we can't fold constants due to either left/right, or both, not being a number
         [else (list operator left right)]))]

    ; ending 'else block', returning original expression if neither num, var, or list was passed to function
    [else expr]))





;;-----------PHASE2: REDUCE IDENTITIES FUNCTION-------------

(define (reduce-identities expr)
  (match expr
    ; Base case: if just humber/symbol then return
    [(? number?) expr]         
    [(? symbol?) expr]
    
    ;Like-Term Extraction, this checks if we're adding two multiplication expressions AND if two symbols in other expression MATCH, AND values are numbers
    ;IF SO, we can combine
    [(list '+ (list '* (? symbol? x) (? number? A)) 
           (list '* (? symbol? y) (? number? B)))
     (if (equal? x y)
         (reduce-identities (list '* (+ A B) x))
         (list '+ (list '* x A) (list '* y B)))]
    

    ; Addition Id
    [(list '+ 0 r)             
     (reduce-identities r)]
    [(list '+ l 0)            
     (reduce-identities l)]

    ;Zero Property
    [(list '* 0 _)              
     0]
    [(list '* _ 0)             
     0]

    ;Mutliplication ID
    [(list '* 1 r)             
     (reduce-identities r)]
    [(list '* l 1)            
     (reduce-identities l)]

    ; recursively function call if its another list
    [(list op l r)              
     (list op
           (reduce-identities l)
           (reduce-identities r))]

    ;invalid expression passed
    [else expr]))






;----------CANONICAL FORM FUNCTION----------

(define (canonicalize expr)
  (match expr
    ;Base Case: atomic values like num/symbol don't need formating, single values so return
    [(? number?) expr]
    [(? symbol?) expr]
    
    ;If list operator is '+' or '*' then we can swap due to commutative properties
    ;if number is left and symbol is right, then swap to canonical form
    [(list (or '+ '*) (? number? n) (? symbol? s))
     (list (first expr) s n)]  
    
    ;everthing is we dont swap
    [(list op left right)
     (list op left right)]
    
    ;invalid expr pass
    [else expr]))





;----------------PHASE3: SIMPLIFY FUNCTION-----------------

(define (simplify expr)
  (match expr
    ; Base case
    [(? number?) expr]
    [(? symbol?) expr]
    
    ; If list, 
    [(list op left right)
     (let* ([simp-left (simplify left)]          
            [simp-right (simplify right)]
            
            ;; Build expression and canonicalize FIRST
            [canonicalized (canonicalize (list op simp-left simp-right))]
            
            ;; Then apply transformations
            [folded (fold-constants canonicalized)]
            [reduced (reduce-identities folded)])
       
       ;; Fixed-point check
       (if (equal? reduced expr)
           reduced
           (simplify reduced)))]
            
    ;  Invlaid pass
    [else expr]))




;;----------------------------TESTS-------------------------

;General Test-------------
(fold-constants '(+ 1 2))
(fold-constants '(+ x (* 2 3)))
(fold-constants '(* (+ 1 2) 5))

(reduce-identities '(+ x 0))       
(reduce-identities '(* y 1))          
(reduce-identities '(* z 0))          
(reduce-identities '(+ (+ x 0) 0))    
(reduce-identities '(+ x 3))

(canonicalize '(+ 3 x))
(canonicalize '(* 5 y))
(canonicalize '(+ 7 z))
(canonicalize '(* 2 a))
(canonicalize '(+ x 3))
(canonicalize '(* y 5))
(canonicalize '(- 3 x))
(canonicalize '(/ 6 y))
(canonicalize '(- 10 z))
(canonicalize 'x)
(canonicalize 5)
(canonicalize '(+ x y))





;Test Inputs/Outputs on Program Assignment----------------
(displayln "Test 1:")
(simplify '(+ x 0))

(displayln "Test 2:")
(simplify '(+ 5 (+ 10 2)))

(displayln "Test 3:")
(simplify '(+ (* 1 x) (* y 0)))

(displayln "Test 4:")
(simplify '(+ 0 (+ 0 (+ 0 z))))

(displayln "Test 5:")
(simplify '(* (+ 1 0) (+ x 0)))

(displayln "Test 6:")
(simplify '(+ (* 1 x) (+ 0 y)))

(displayln "Test 7:")
(simplify '(- 20 (/ (+ (* 4 5) (* 2 5)) (- 8 2))))

(displayln "Test 8:")
(simplify '(+ (* 2 x) (* 3 x)))

(displayln "Test 9:")
(simplify '(+ (* 2 x) (* 3 y)))

(displayln "My Test:")
(simplify '(+ (* x (+ 1 0)) (* y 3)))