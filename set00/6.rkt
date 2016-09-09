;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; quadratic-root : Number Number Number -> Number Number
; GIVEN: the coefficients in a quadratic equation
; RETURNS: one of the roots (or solutions) of the equation
; Examples:
; (quadratic-root 7 -10 -5) => 1.8
; (quadratic-root 1 -4 0) => 4

(define
  (quadratic-root a b c)
  (/ 
   (+
    (- 0 b)
    (sqrt
     (-
      (sqr b)
      (* 4 a c))))
   (* 2 a)))