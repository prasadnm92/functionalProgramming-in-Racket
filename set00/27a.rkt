;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 27a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;; drawStr : ListOfString -> Image
;; GIVEN: a list of string constants
;; RETURNS: an image of the combined text of those strings,
;;          separated by spaces
;; EXAMPLES:
;;  (drawStr (list "Northeastern" "University"))
;;  (drawStr (list "Lets" "go" "HUSKIES" "!!!!!!!!!!"))

(define (drawStr strList)
  (cond
    [(empty? strList) "Enter non-empty list"]
    [(empty? (rest strList)) (text (first strList) 12 "red")]
    [else (beside (text (string-append (first strList) " ") 12 "red") (drawStr (rest strList)))]))