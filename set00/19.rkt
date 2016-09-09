;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |19|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; rect: Number Number -> Rectangle
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

; rel-rec-sequence: Number Number -> Image
; GIVEN: two numbers representing width and proportion
; RETURNS: an image of a rectangle with the given width and
;          height proprtional to the width
; EXAMPLES: (rel-rec-sequence 6 0.5) => rectangle of width=6
;                                                    height=(6*.5)=3

(define (rel-rec-sequence w p)
  (rect w (* w p)))