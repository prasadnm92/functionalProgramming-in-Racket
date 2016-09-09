;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |13|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; point : Number Number -> Point
; GIVEN: the co-ordinates of the point in 2 dimensions
; RETURNS: a structure data type with the co-ordinates in it
; Funtions defined:
; make-point : creates a structure with values given as arguments
; point? : returns true if the argument is of the type point
;            else returns false
; point-x / point-y : returns the value at the respective field names

;Examples:
; (make-point true false) => (make-point true false)
; (point-x (make-point true false)) => true

(define-struct point (x y))