;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 21a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
#|
(require "extras.rkt")
(provide person-image)
(provide head)
(provide hand)
(provide body)
(provide leg)
(provide leg-gap)
|#
; rect: Number Number -> Image
; GIVEN: takes two numbers for the sides of a rectangle
; RETURNS: an image of the rectangle having those two dimensions
;          in green solid color
; EXAMPLE: (rect 2 4) => a rectangle with sides 2 and 4 pixels
(define (rect a b)
  (rectangle a b "solid" "green"))
(define (empty-rect a b)
  (rectangle a b "solid" "white"))

; circ: Number -> Image
; GIVEN: takes a number for the radius of a circle
; RETURNS: an image of the circle having that radius
;          in green solid color
; EXAMPLE: (circ 2) => a circle of radius 2 pixels
(define (circ r)
  (circle r "solid" "green"))

(define-struct person (f-name l-name age height weight))
; person is a
;  (make-person (String String PosInt PosInt PosInt))
;  Interpretation:
;  -f-name is the first name of the person
;  -l-name is the last name of the person
;  -age is the age of the person
;  -height is the height of the person in pixels
;  -weight is the weight of the person in terms of pixels
;     i.e., the more pixels, the heavier the paerson is 

;(provide (struct-out person))

; person-image: Person -> Image
; GIVEN: takes a Person structure
; RETURNS: an image of the person proportional to his height AND weight
; EXAMPLES:
;  (person-image (make-person "John" "Marshal" 45 80 40))
;  (person-image (make-person "Henry" "Stinson" 25 80 20))
;  (person-image (make-person "Micheal" "Doughlas" 60 80 160))
;  (person-image (make-person "Jim" "Parson" 10 40 20))
;  (person-image (make-person "George" "Micheal" 42 160 40))

(define (person-image p1)
  (above
   (head p1)
   
   (beside
    (hand p1)
    (body p1)
    (hand p1))
   
   (beside
    (leg p1)
    (leg-gap p1)
    (leg p1))
   (text (string-append (person-f-name p1) " " (person-l-name p1)) 12 "blue")))

; head: Person -> Image
; GIVEN: a Person Structure
; RETURNS: image of the Person's head
(define (head p1)
  (circ (* .25 (person-weight p1))))

; hand: Person -> Image
; GIVEN: a Person Structure
; RETURNS: image of one of the hands of the Person
(define (hand p1)
  (above
     (rect (* .25 (person-height p1)) (* .125 (person-weight p1)))
     (empty-rect (* .25 (person-height p1)) (- (* .5 (person-height p1)) (* .125 (person-weight p1))))))

; body: Person -> Image
; GIVEN: a Person Structure
; RETURNS: image of the Person's body
(define (body p1)
  (rect (* .50 (person-weight p1)) (* .5 (person-height p1))))

; leg: Person -> Image
; GIVEN: a Person Structure
; RETURNS: image of one the legs of the Person
(define (leg p1)
  (rect (* .125 (person-weight p1)) (* .25 (person-height p1))))

; leg-gap: Person -> Image
; GIVEN: a Person Structure
; RETURNS: image of the gap between the Person's legs
(define (leg-gap p1)
  (empty-rect (- (* .50 (person-weight p1)) (* 2 (* .125 (person-weight p1)))) (* .125 (person-weight p1))))

