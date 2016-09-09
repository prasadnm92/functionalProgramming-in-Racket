;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |20|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; rect: Number Number String -> Image
; GIVEN: takes two numbers for the sides of a rectangle
;        a string for its color
; RETURNS: an image of the rectangle having those two dimensions
;          in the given solid color
; EXAMPLE: (rect 2 4 "green") => a rectangle with sides 2 and 4 pixels
(define (rect a b c)
  (rectangle a b "solid" c))

; circ: Number -> Image
; GIVEN: takes a number for the radius of a circle
; RETURNS: an image of the circle having that radius
;          in green solid color
; EXAMPLE: (circ 2) => a circle of radius 2 pixels
(define (circ r)
  (circle r "solid" "green"))

; human: None -> Image
; GIVEN: takes no parameters
; RETURNS: an image shaped like a human

(above
 (circ 10)
 (beside
  (above
   (rect 20 5 "green")
   (rect 20 35 "white"))
  (rect 20 40 "green")
  (above
   (rect 20 5 "green")
   (rect 20 35 "white")))
 (beside
  (rect 5 30 "green")
  (rect 10 30 "white")
  (rect 5 30 "green")))
