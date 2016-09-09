;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps03-inventory-qualification) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps03-inventory-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "inventory.rkt")  

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define reorder-status-1 (make-reorder 2 5))
(define book-1 (make-book 15 "How to Design Programs" "Felleisen et al." "MIT Press" 59 49 100 reorder-status-1 1/12))
(define inventory-1 (list book-1))
(define line-item-1 (make-line-item 15 2))
(define order-1 (list line-item-1))

(define-test-suite inventory-tests
  ;; this only tests to see if required functions were provided.  
  ;; Does not test correctness AT ALL
  (check-provided (inventory-potential-profit inventory-1))
  (check-provided (inventory-total-volume inventory-1)) 
  (check-provided (price-for-line-item inventory-1 line-item-1)))