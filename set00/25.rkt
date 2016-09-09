;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |25|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; trueList? : ListOfBoolean -> Boolean
; GIVEN: takes a list of Boolean values
; RETURNS: true iff all the values in ListOfBoolean are true
; EXAMPLES:
;  (trueList? (list true false true true)) => false
;  (trueList? (list true true)) => true

(define (trueList? listOfBool)
  (cond
     [(empty? listOfBool) false]
     [(empty? (rest listOfBool)) (first listOfBool)]
     [else (and (first listOfBool) (trueList? (rest listOfBool)))]))