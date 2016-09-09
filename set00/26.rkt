;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |26|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; point : Number Number -> Point
; GIVEN: the co-ordinates of the point in 2 dimensions
; RETURNS: a structure data type with the co-ordinates in it

(define-struct point (x y))

; circ: Number -> Image
; GIVEN: takes a number for the radius of a circle
; RETURNS: an image of the circle having that radius
;          in green solid color
; EXAMPLE: (circ 2) => a circle of radius 2 pixels
(define (circ r)
  (circle r "solid" "blue"))


;plot : ListOfPoints -> Scene
;GIVEN: takes a list of points with X and Y coordinates to be plotted
;       Here, each element is a Point structure
;RETURNS: a 300X300 scene with the plotted points in blue circles
;         of 10 radius
;EXAMPLES:
;(plotPts (list (make-point 10 20) (make-point 150 250) (make-point 100 200)))

(define (plotPts listOfPts1)
  (plot listOfPts1 (empty-scene 300 300)))

;plot : ListOfPoints Scene -> Scene
;GIVEN: takes a ListOfPoints and a Scene
;RETURNS: a Scene with the plotted points in blue circles
;         of 10 radius

(define (plot listOfPts scene)
  (cond
    [(empty? listOfPts) "Enter a non-empty list"]
    [(empty? (rest listOfPts)) (place-image (circ 10)
                                            (point-x (first listOfPts))
                                            (point-y (first listOfPts))
                                            scene)]
    [else (plot (rest listOfPts) (place-image (circ 10)
                                            (point-x (first listOfPts))
                                            (point-y (first listOfPts))
                                            scene))]))