;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |32|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; point : Number Number -> Point
; GIVEN: the co-ordinates of the point in 2 dimensions
; RETURNS: a structure data type with the co-ordinates in it

(define-struct point (x y))

; manhtnDist : Point -> Number
; GIVEN: takes a Point with X and y coordinates
; RETURNS: the manhattan distance of the point from (0,0)
; EXAMPLES:
;; (manhtnDist (make-point 2 3)) => 5
;; (manhtnDist (make-point 2.5 31)) => 77.5

(define (manhtnDist pt)
  (+ (point-x pt) (point-y pt)))

; sumDist : ListOfPoints -> Number
; GIVEN: takes a list of Points
; RETURNS: the sum of distances of all the points in the list
;          from (0,0)
; EXAMPLES:
;;(sumDist (list
;;          (make-point 2 3)
;;          (make-point 2.5 31)
;;          (make-point 10 21)
;;          (make-point 12 56.34)))
          
(define (sumDist listOfPts)
  (cond
    [(empty? listOfPts) "Enter a non-empty list"]
    [(empty? (rest listOfPts)) (manhtnDist (first listOfPts))]
    [else (+ (manhtnDist (first listOfPts)) (sumDist (rest listOfPts)))]))