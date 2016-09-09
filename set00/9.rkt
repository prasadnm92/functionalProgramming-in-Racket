;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; is-even? : Number -> Booleam
; GIVEN: a number to checked
; RETURNS: true if the number is even, false otherwise
; Examples:
; (is-even? 3) => false
; (is-even? -2) => true

(define
  (is-even? n)
  (= 0 (remainder n 2)))