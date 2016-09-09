;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |18|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; rect: Number Number -> Image
; GIVEN: takes two numbers for the sides of a rectangle
; RETURNS: an image of the rectangle having those two dimensions
;          in blue solid color

(define (rect a b)
  (rectangle a b "solid" "blue"))
#|
; EXAMPLE: (rect 2 4) => a rectangle with sides 2 and 4 pixels
(rect 2 4)
(rect 4 8)
(rect 8 16)
(rect 16 32)
(rect 32 64)
(rect 64 128)
|#

; Formula for n-th element in the sequence:
; (rect expt n) (expt 2 (+ n 1)))

; rec-sequence: PosInt -> Image
; GIVEN: a positive integer representing a position in a sequence
; RETURNS: an image of a rectangle with sides equal to
;         (2^n) and (2^(n+1)) pixels

; EXAMPLES:
; (rec-sequence 1) => rectangle of sides 2 and 4 pixels
; (rec-sequence 10) => rectangle of sides 2^10 and 2^11 pixels

(define (rec-sequence n)
  (rect (expt 2 n) (expt 2 (+ n 1))))