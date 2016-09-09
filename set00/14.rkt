;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |14|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; student : Number Number -> Student
; GIVEN: the co-ordinates of the point in 2 dimensions
; RETURNS: a structure data type with the co-ordinates in it
; Funtions defined:
; make-student : creates a structure with values given as arguments
; student? : returns true if the argument is of the type student
;            else returns false
; student-id / student-name / student-major : returns the value
;                                  at the respective field names


(define-struct student (id name major))