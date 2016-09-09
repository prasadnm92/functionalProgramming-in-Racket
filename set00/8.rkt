;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; circle-area : Number -> Number
; GIVEN: radius of a circle
; RETURNS: the area of that circle
; Examples:
; (circle-area 1) => 3.141592653589793
; (circle-area 5) => 78.53981633974483
; (circle-area 7) => 153.93804002589985

(define
  (circle-area r)
  (* pi r r))