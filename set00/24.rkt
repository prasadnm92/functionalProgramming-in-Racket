;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |24|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; pdt : List -> Number
; Returns the product of the numbers in the given list
; and returns a zero for an empty list
; EXAMPLES: 
; (pdt empty) = 0
; (pdt (list 1)) = 1
; (pdt (list 1 2 3 4)) = 24
(define (pdt lst)
  (cond
    [(empty? lst) 0]
    [(empty? (rest lst)) (first lst)]
    [else (* (first lst) (pdt (rest lst)))]))

; You can use wrapper function as well.
; Wrap the actual recurssion in 1 function and call that
; from the main function after checking for an empty case

(define (pdt1 lst)
  (cond
    [(empty? lst) 0]
    [else (actual-pdt1 lst)]))

(define (actual-pdt1 lst)
  (cond
    [(empty? lst) 1]
    [else (* (first lst) (actual-pdt1 (rest lst)))]))