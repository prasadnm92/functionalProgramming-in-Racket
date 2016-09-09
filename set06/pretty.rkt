;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;;Purpose Statement
;The program is called pretty.rkt and it contains a pretty-printer for Exprs.

(require rackunit)
(require "extras.rkt")

;;PROVIDING FUNCTIONS
(provide expr-to-strings
         make-sum-exp
         sum-exp-exprs
         make-mult-exp
         mult-exp-exprs)

;;CONSTANTS

(define PLUS "+")
(define MULT "*")
(define SPACE " ")
(define OPEN-P "(")
(define CLOSE-P ")")
(define EMPTY "")
(define INCR 3)
(define INITIAL-MARGIN 0)
(define ZERO 0)
(define ONE 1)
(define TEN 10)
(define SUM-START (string-append OPEN-P PLUS))
(define MULT-START (string-append OPEN-P MULT))
(define ERROR "Not Enough Space")


;;---------------------DATA DEFINITIONS-------------------------------

;;STRUCTURE DEFINTION FOR sum-exp:
(define-struct sum-exp (exprs))
;;CONSTRUCTOR TEMPLATE:
;A SumExp is a (make-sum-exp NELOExpr)
;;INTERPRETATION:
; exprs - Its a list of operands required for sum operation.
;;TEMPLATE:
;sum-exp-fn : SumExp -> ??
#;(define (sum-exp-fn exp)
    (... (sum-exp-exprs exp)))

;;STRUCTURE DEFINTION FOR mult-exp:
(define-struct mult-exp (exprs))
;;CONSTRUCTOR TEMPLATE:
; A MultExp is a (make-mult-exp NELOExpr)
;;INTERPRETATION:
; exprs - Its a list of operands required for multiplication operation.
;;TEMPLATE:
;mult-exp-fn : MultExp -> ??
#;(define (mult-exp-fn exp)
    (... (mult-exp-exprs exp)))

;;An Expr is one of
; -- Integer
; -- (make-sum-exp NELOExpr)
; -- (make-mult-exp NELOExpr)
;;INTERPRETATION
; Integer -- It indicates an integer value
; sum-exp - It represents a sum
; mult-exp - It represents a multiplication
;;TEMPLATE:
;expr-fn : Expr -> ??
#;(define (expr-fn exp)
    (cond
      [(integer? exp) ...]
      [(sum-exp? exp) (... (loexpr-fn (sum-exp-exprs)))]
      [(mult-exp? exp) (... (loexpr-fn (mult-exp-exprs)))]))

;;--------------------------------------------------------------------------

;;Two Different Data Definitions for non-empty lists.

; A LOExpr is one of
; -- empty
; -- (cons Expr LOExpr)
;;TEMPLATE:
;loexpr-fn : LOExpr -> ??
#;(define (loexpr-fn expr)
    (cond
      [(empty? expr) ...]
      [else (...
             expr-fn (first expr)
             loexpr-fn (rest expr))]))

;; A NELOExpr1 is a non-empty LOExpr
;  -- (cons Expr LOExpr)
;;TEMPLATE:
;nelst1-fn : NELOExpr1 -> ??
#;(define (nelst1-fn nelo-expr)
    (... (expr-fn (first nelo-expr))
         (loexpr-fn (rest nelo-expr))))

;; A NELOExpr2 is one of
;  -- (cons Expr empty)
;  -- (cons Expr NELOExpr2)
;;TEMPLATE:
;nelst2-fn : NELOExpr2 -> ??
#;(define (nelst2-fn nelo-expr)
    (cond
      [(empty? (rest nelo-expr)) ...]
      [else (... (expr-fn (first nelo-expr))
                 (nelst2-fn (rest nelo-expr)))]))

;---------------------------------------------------------------------------

;; A ListOf<String> (LOS) is one of
;  -- empty
;  -- (cons String ListOf<String>)
;;TEMPLATE:
;los-fn : LOS -> ??
#;(define (los-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (first lst)
                 (los-fn (rest lst)))]))

;---------------------------------------------------------------------------

;Examples  for testing purposes:

(define example1 (make-sum-exp (list 1 2 (make-sum-exp (list 3 4)) 5)));w=17
(define example1-output '("(+ 1 2 (+ 3 4) 5)"))

(define example2 (make-sum-exp (list 13 46 (make-sum-exp (list 1 2 3)))));w=19
(define example2-output (+ 13 46 (+ 1 2 3)));w=20

(define example3 (make-sum-exp (list 13 46 (make-sum-exp (list 1 2 3)) 50)));22
(define example3-output '("(+ 13" "   46" "   (+ 1 2 3)" "   50)"));w=20

(define example4 (make-sum-exp (list 12 23 56)));w=12

(define example5 (make-mult-exp 
                  (list 222 444 (make-mult-exp 
                                 (list 22 33 4444 
                                       (make-mult-exp 
                                        (list 4 6666 8888)))) 33 44)));w=48
(define example5-output '("(* 222"
                          "   444"
                          "   (* 22"
                          "      33"
                          "      4444"
                          "      (* 4"
                          "         6666"
                          "         8888))"
                          "   33"
                          "   44)"))

(define mul1 (make-mult-exp (list 12 34 (make-mult-exp 
                                         (list 2345 5767)) 345)));w=27
(define mul2 (make-mult-exp (list 12 34 (make-sum-exp 
                                         (list 2345 5767)) 345)));w=27
(define sum1 (make-sum-exp (list 2345 5767)));w=13
(define sum2 (make-sum-exp (list 234 567 (make-mult-exp (list 2)))));w=17
(define sum3 (make-sum-exp 
              (list (make-sum-exp 
                     (list (make-mult-exp (list 3 4 5)) 6)) 7)));w=21

;;----------------------------DISPLAY FUNCTION------------------------------
;display-expr : Expr NonNegInt -> Void
;GIVEN : an expression and a width
;RETURNS : nothing, but displays the (expr-to-strings exp w) of the given
#;(define (display-expr expr n)
    (display-strings! (expr-to-strings expr n)))


;;-------------------------END OF DATA DEFINITIONS--------------------------

;expr-to-strings : Expr NonNegInt -> LOS
;GIVEN : An expression and a width
;RETURNS :  A representation of the expression as a sequence of lines, with
;           each line represented as a string of length not greater than 
;           the width.
;EXAMPLES :
;  see tests
;STRATEGY :Function Composition
(define (expr-to-strings exp w)
  (expr-string exp w INITIAL-MARGIN))

;Tests :
(begin-for-test 
  (check-equal? (expr-to-strings example5 50)
                '("(* 222 444 (* 22 33 4444 (* 4 6666 8888)) 33 44)")
                "Failed to convert the expression into strings within the
                 given width")
  (check-equal? (expr-string example5 20 0)
                example5-output
                "Failed to convert the expression into strings within the
                 given width")
  (check-error (expr-to-strings example5 5)
               ERROR)
  (check-equal? (expr-to-strings sum2 20) 
                '("(+ 234 567 (* 2))")
                "Failed to print the entire expression in onle line when enough
                width is supplied")) 

;.....................

;expr-string : Expr NonNegInt NonNegInt -> LOS
;GIVEN : An expression 'exp', Width 'w' available, an initial context 'ctxt'.
;WHERE : 'ctxt' keeps track of the no. of spaces from left margin.
;RETURNS : LOS that has been obatined from the given expression, after checking
;          if it fits in a single line or not.
;EXAMPLES:
;   (expr-string example1 20 0) => example1-output
;   (expr-string example3 20 0) => example3-output
;STRATEGY : Function Composition
(define (expr-string exp w ctxt)
  (if (expr-fits? exp (- w ctxt))
      (list (string-append (print-space ctxt)
                           (expr-in-one-line exp ctxt)))
      (split-expr exp w ctxt)))

;.....................

;expr-fits? : Expr NonNegInt -> Boolean
;GIVEN :  An Expression 'exp', Width 'w' available.
;RETURNS : True iff expression 'exp' length is less than that of width.
;EXAMPLES:
;    (expr-fits? example1 20) => True
;    (expr-fits? example1 5) => False
;STRATEGY : Function Composition
(define (expr-fits? exp w)
  (<= (expr-length exp) w))

;expr-length : Expr -> Integer 
;GIVEN : An Expr 'exp'
;RETURNS : length of the integer, incase the expression is an Integer, length
;          of the expression  if its a sum or mult expression.
;EXAMPLES :
;   (expr-length mul1) => 27
;   (expr-length example5) => 48
;STRATEGY : Structural Decomposition on exp : Expr
(define (expr-length exp)
  (cond
    [(integer? exp) (string-length (number->string exp))]
    [(sum-exp? exp) (+ (length-of-expr (get-sum-exp exp)) INCR)]
    [(mult-exp? exp) (+ (length-of-expr (get-mult-exp exp)) INCR)]))

;get-sum-exp : SumExp -> Expr
;GIVEN : An expression 'exp'
;RETURNS : a sum expression
;EXAMPLE : 
;    (get-sum-exp sum1) => (2345 5767)
;STRATEGY : Structural Decompositon on exp : SumExp
(define (get-sum-exp exp)
  (sum-exp-exprs exp))

;get-sum-exp : MultExp -> Expr
;GIVEN : An expression 'exp'
;RETURNS : a mult expression
;EXAMPLE : 
;     (get-mult-exp mul1) => (12 34 (make-mult-exp (list 2345 5767)) 345)
;STRATEGY : Structural Decompositon on exp : MultExp
(define (get-mult-exp exp)
  (mult-exp-exprs exp))

;length-of-expr : NELO<Expr> -> NonNegInt
;GIVEN : A Non empty list of expression 'exp-list'
;RETURNS : Length of the expression
;EXAMPLE:
;    (length-of-expr (list 22 33 44)) => 9
;STRATEGY : HOFC
(define (length-of-expr exp-list)
  (foldr
   (;Expr NonNegInt -> NonNegInt
    ;GIVEN : An Expr and length of the expression traversed so far.
    ;RETURNS : total length of expression traversed till now.
    lambda (each-exp length-so-far)
     (+ (expr-length each-exp) length-so-far ONE))
   ZERO
   exp-list))

;.....................

;expr-in-one-line : Expr NonNegInt -> String
;GIVEN : An Expr 'exp' and a context 'ctxt'.
;WHERE :  Context 'ctxt' which keep tracks of no.of spaces 
;        from left margin.
;RETURNS : A String obtained from the given expression 'exp'.
;EXAMPLE:
;    (expr-in-one-line example1 0) => "(+ 1 2 (+ 3 4) 5)"
;STRATEGY : Structural Decomposition on exp : Expr

(define (expr-in-one-line exp ctxt)
  (cond
    [(integer? exp) (string-append (number->string exp))]
    [(sum-exp? exp) (string-append 
                     SUM-START 
                     (expr-list-in-one-line (get-sum-exp exp) ZERO)
                     CLOSE-P)]
    [(mult-exp? exp) (string-append 
                      MULT-START 
                      (expr-list-in-one-line (get-mult-exp exp) ZERO)
                      CLOSE-P)]))

;expr-list-in-one-line : NELO<Expr> NonNegInt -> String
;GIVEN : A non empty list of expressions, context 'ctxt'.
;WHERE : Context 'ctxt' which keep tracks of the space from left margin
;RETURNS : An String obtained from the non empty list of expression.
;EXAMPLE :
;   (expr-list-in-one-line (list 22 33 44) 0) => "22 33 44" 
;STRATEGY : HOFC
(define (expr-list-in-one-line exp-list ctxt)
  (foldl
   (;Expr String -> String
    ;GIVEN : an expression from given list and a string of expressions seen 
    ;        so far.
    ;RETURNS : a string of current expression added to the previous string.
    lambda (each-exp string-so-far)
     (string-append string-so-far SPACE (expr-in-one-line each-exp ctxt)))
   EMPTY
   exp-list))

;print-space : NonNegInt -> String
;GIVEN : A Context 'num'
;WHERE : Context 'num', that keep tracks of no. of spaces from left margin.
;RETURNS : A String obtained depending on the Context.
;EXAMPLE:
;    (print-space 3) => "   "
;STRATEGY : Function Composition
(define (print-space num)
  (make-string num #\space))

;;--------------------------------------------------------------------------

;split-expr : Expr NonNegInt NonNegInt -> LOS
;GIVEN : An Expr 'exp' , Width 'w' available no. of spaces, context 'ctxt'
;WHERE : Context 'ctxt', that keep tracks of the space from left margin.
;RETURNS : ListOf<String> based on the given expression.
;EXAMPLES:
;    See Test Cases
;STRATEGY : Structural Decomposition on exp : Expr
(define (split-expr exp w ctxt)
  (cond
    [(integer? exp) (error ERROR)]
    [(sum-exp? exp) (split-sum exp w ctxt)]
    [(mult-exp? exp) (split-mult exp w ctxt)]))

(begin-for-test 
  (check-equal? (split-expr example2 10 0)
                '("(+ 13" "   46" "   (+ 1" "      2" "      3))")
                "Failed to split  the expression when width was less than
                 the expression")
  (check-equal? (split-list (mult-exp-exprs (last (sum-exp-exprs sum2)))
                            12 6 "(*")
                '("(* 2")
                "Failed to split an expression that exceeds the width when
                  displayed in one line"))


;split-sum : Expr NonNegInt NonNegInt -> LOS
;GIVEN : An Expression 'exp', Width 'w' available.
;WHERE : Context 'ctxt' which keep tracks of no. of spaces from left margin.
;RETURNS : LOS obatined from the expressions traversed so far.
;EXAMPLE:
;    (split-sum (split-sum example1 20 0)) =>
;    ("(+ 1" "   2" "   (+ 3 4)" "   5)")
;STRATEGY : Structural Decomposition on exp : SumExp
(define (split-sum exp w ctxt)
  (split-list (sum-exp-exprs exp) w ctxt (string-append (print-space ctxt)
                                                        SUM-START)))

;split-mult : Expr NonNegInt NonNegInt -> LOS
;GIVEN : An Expression 'exp', Width 'w' available, context 'ctxt'. 
;WHERE : Context 'ctxt' which keep tracks of no of spaces from left margin.
;RETURNS : LOS obatined from the given expression.
;EXAMPLE:
;   (split-mult mul1 20 0) =>
;   ("(* 12" "   34" "   (* 2345 5767)" "   345)")
;STRATEGY : Structural Decomposition on exp : MultExp
(define (split-mult exp w ctxt)
  (split-list (mult-exp-exprs exp) w ctxt (string-append (print-space ctxt)
                                                         MULT-START)))

;.....................

;split-list : NELO<Expr> NonNegInt NonNegInt String -> LOS
;GIVEN : A non empty ListOf<Expr> 'exp-list' ,  Width 'w' available, 
;        context 'ctxt' and a String.
;WHERE : Context 'ctxt' which keep tracks of number of spaces from left margin.
;RETURNS : A LOS obtained from the given expression.
;EXAMPLE:
;    (split-list (list 22 33 44) 20 0 " ") =>
;    ("  22" "   33" "   44)")
;STRATEGY : Structural Decomposition on exp-list : NELO<Expr>
(define (split-list exp-list w ctxt start)
  (append (add-open-parenthesis (first exp-list) w ctxt start)
          (check-rest-empty (rest exp-list) w (+ INCR ctxt))))

;add-open-parenthesis : Expr NonNegInt NonNegInt String -> LOS
;GIVEN : An Expression 'exp', Width 'w' available, context 'ctxt; 
;        and a String.
;WHERE : Context 'ctxt' which keep tracks of no. of spaces from left margin.
;RETURNS : A LOS obtained from the given expression.
;EXAMPLE :
;    (add-open-parenthesis example1 20 0 " ") =>
;    ("  (+ 1 2 (+ 3 4) 5)")
;STRATEGY : Function Composition
(define (add-open-parenthesis expr w ctxt start)
  (cons (string-append start SPACE
                       (first (expr-string expr (- w ctxt) ZERO)))
        (rest (expr-string expr w (+ ctxt INCR)))))

;.....................

;check-rest-empty: ListOf<Expr> NonNegInt NonNegInt -> LOS
;GIVEN : A List of expression 'exp-list',Width 'w' available, context 'ctxt' 
;WHERE : Context 'ctxt' which keep tracks of no. spaces from left margin.
;RETURNS : A LOS obtained from the given expression.
;EXAMPLE :
;    (check-rest-empty (list 22 33 44) 20 0) =>
;    ("22" "33" "44)")
;STRATEGY : Structural Decomposition on exp-list : ListOf<Expr>
(define (check-rest-empty exp-list w ctxt)
  (cond
    [(empty? exp-list) empty]
    [else (split-rest exp-list w ctxt)]))

;.....................

;split-rest : NELO<Expr> NonNegInt NonNegInt -> LOS
;GIVEN: A List of expression 'exp-list',Width 'w' available,context 'ctxt'.
;WHERE: Context 'ctxt' which keep tracks of no. of spaces from left margin.
;RETURNS:  A LOS obtained from the given expression.
;EXAMPLE:
;    (split-rest (list 22 33 44)20 0) =>
;    ("22" "33" "44)")
;STRATEGY: Structural Decomposition on exp-list : NELO<Expr>
(define (split-rest exp-list w ctxt)
  (cond
    [(empty? (rest exp-list)) (add-close-parenthesis (first exp-list) w ctxt)]
    [else (append (expr-string (first exp-list) w ctxt)
                  (split-rest (rest exp-list) w ctxt))]))

#;(expr-to-strings-from (split-rest (rest exp-list) w ctxt) 
                      w
                      ctxt 
                      (expr-string (first exp-list) w ctxt))


;add-close-parenthesis : Expr NonNegInt NonNegInt -> LOS
;GIVEN : An expression'expr', Width 'w' available, context 'ctxt'.
;WHERE : Context 'ctxt' which keep tracks of the space from left margin.
;RETURNS : A LOS obtained from the given expression.
;EXAMPLE :
;    (add-close-parenthesis example1 20 0) =>
;    ("(+ 1 2 (+ 3 4) 5))")
;STRATEGY : Function Composition
(define (add-close-parenthesis expr w ctxt)
  (append (first-last (expr-string expr (- w ONE) ctxt))
          (list 
           (string-append (last (expr-string expr (- w ONE) ctxt))
                          CLOSE-P))))

;.....................

;last : ListOf<X> -> X
;GIVEN : a List of X
;RETURNS : last element of the given list
;EXAMPLE :
;    (last (list 22 33 44)) => 44 
;STRATEGY : Function Composition
(define (last lst)
  (first (reverse lst)))

;first-last : ListOf<X> -> ListOf<X>
;GIVEN: A List X
;RETURNS : A list of X, where it contains all the element, except last one.
;EXAMPLE :
;    (first-last (list 22 33 44)) => (22 33)
;STRATEGY: Function Composition
(define (first-last lst)
  (reverse (rest (reverse lst))))



