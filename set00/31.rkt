;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |31|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; circ: Number -> Image
; GIVEN: takes a number for the radius of a circle
; RETURNS: an image of the circle having that radius
;          in brown outline color
; EXAMPLE: (circ 2) => a circle of radius 2 pixels
(define (circ r)
  (circle r "outline" "brown"))

; circles : listOfNumbers -> Image
; GIVEN: a list of NonNegNumbers
; RETURNS: a list of images of circles of radius equal to the values in the list
; EXAMPLES:
;; (circles (list 1 2 3 4 5 6))
;; (circles (list 10.5 6 7.8 15 22 17.3))

(define (circles listOfNum)
  (cond
    [(empty? listOfNum) "enter a non-empty list"]
    [(empty? (rest listOfNum)) (cons (circ (first listOfNum)) empty)]
    [else (cons
           (circ (first listOfNum))
           (circles (rest listOfNum)))]))