;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; sum-of-max2 : Number Number Number -> Number
; GIVEN: any three numbers
; RETURNS: the sum of the larger two numbers
; Examples:
; (sum-of-max2 3 5 7) => 12
; (sum-of-max2 3 5 -7) => 8

(define
  (sum-of-max2 a b c)
  (-
   (+ a b c)
   (min a b c)))