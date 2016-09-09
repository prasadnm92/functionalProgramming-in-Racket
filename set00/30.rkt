;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |30|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; neg-list : listOfBoolean -> listOfBoolean
; GIVEN: a list of Boolean values
; RETURNS: another list of Boolean values with the
;          corrosponding negated values
; EXAMPLES:
;; (neg-list (list true false true)) => (cons false (cons true (cons false empty)))
;; (neg-list (list true true false false true)) => (cons false (cons false (cons true (cons true (cons false empty)))))
(define (neg-list listOfBool)
  (cond
    [(empty? listOfBool) "enter a non-empty list"]
    [(empty? (rest listOfBool)) (cons
                                 (not (first listOfBool))
                                 empty)]
    [else (cons
           (not (first listOfBool))
           (neg-list (rest listOfBool)))]))