;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require "extras.rkt")

(provide
 initial-machine
 machine-next-state
 machine-chocolates
 machine-carrots
 machine-bank)

(define-struct machine (chocolates carrots bank tray change))
;;a Machine is a (make-machine NonNegInt NonNegInt NonNegInt)
;;Interpretation:
;; --chocolates is the number of chocolate bars left in the machine
;; --carrots is the number of packages of carrot sticks left in the machine
;; --bank is the amount of money in the machine's bank, in cents
;; --tray is the amount of money the customer has inserted into snack machine.
;; --change is the amount returned to the customer,
;;      after release or a successful purchase

;;A CustomerInput is one of
;; -- a PosInt        interp: insert the specified number of cents
;; -- "chocolate"     interp: request a chocolate bar
;; -- "carrots"       interp: request a package of carrot sticks
;; -- "release"       interp: return all the coins that the customer has put in

;;initial-machine : NonNegInt NonNegInt-> Machine
;; GIVEN: the number of chocolate bars and
;;        the number of packages of carrot sticks
;; RETURNS: a machine loaded with the given number of chocolate bars and
;;         carrot sticks, with an empty bank.
;; EXAMPLES:
;;   (initial-machine 20 30) => (make-machine 20 30 0 0 0)
;;   (initial-machine 0 0) => (make-machine 0 0 0 0 0)

(define (initial-machine choc carr)
  (make-machine choc carr 0 0 0))

;;machine-next-state : Machine CustomerInput -> Machine
;;GIVEN: a machine state and a customer input
;;RETURNS: the state of the machine that should follow the customer's input
;;EXAMPLES:
;;  (machine-next-state (make-machine 6 7 400 200) "chocolate") 
;;                        => (make-machine 5 7 575 0 25)
;;  (machine-next-state (make-machine 6 7 400 200) "carrots") 
;;                        => (make-machine 6 6 470 0 130)
;;  (machine-next-state (make-machine 6 7 400 200) "release")
;;                        => (make-machine 6 7 400 0 200)
;;STRATEGY: Cases on CustomerInput

(define (machine-next-state machine1 cust-ip)
  (cond
    [(number? cust-ip) (make-machine
                        (machine-chocolates machine1)
                        (machine-carrots machine1)
                        (machine-bank machine1)
                        (+ (machine-tray machine1) cust-ip)
                        0)]
    [(string=? cust-ip "chocolate") (dispense-choc machine1)]
    [(string=? cust-ip "carrots") (dispense-carr machine1)]
    [(string=? cust-ip "release") (release-tray machine1)]))

;;dispense-choc : Machine -> Machine
;;GIVEN: takes a machine state before the request for a chocolate
;;RETURNS: the machine state after dispensing a chocolate and returning the
;;         change (if any). Returns the same state if there is no stock
;;EXAMPLES:
;;  (dispense-choc (make-machine 30 50 0 200 0)) 
;;                        => (make-machine 29 50 175 0 25)
;;  (dispense-choc (make-machine 30 50 0 100 0))
;;                        => (make-machine 30 50 0 100 0)
;;STRATEGY: Structure Decomposition

(define (dispense-choc machine1)
  (if
   (and
    (>= (machine-tray machine1) 175)
    (> (machine-chocolates machine1) 0))
   (make-machine 
    (- (machine-chocolates machine1) 1)
    (machine-carrots machine1)
    (+ (machine-bank machine1) 175)
    0
    (- (machine-tray machine1) 175))
   machine1))

(check-expect (dispense-choc (make-machine 30 50 0 200 0))
              (make-machine 29 50 175 0 25))
(check-expect (dispense-choc (make-machine 30 50 0 100 0))
              (make-machine 30 50 0 100 0))

;;dispense-carr : Machine -> Machine
;;GIVEN: takes a machine state before the request for a package of carrots
;;RETURNS: the machine state after dispensing a package and returning the
;;         change (if any). Returns the same state if there is no stock
;;EXAMPLES:
;;  (dispense-carr (make-machine 30 50 0 50 0)) => (make-machine 30 50 0 50 50)
;;  (dispense-carr (make-machine 30 50 0 100 0)) => (make-machine 30 49 70 0 30)
;;STRATEGY: Structure Decomposition

(define (dispense-carr machine1)
  (if
   (and
    (>= (machine-tray machine1) 70)
    (> (machine-carrots machine1) 0))
   (make-machine 
    (machine-chocolates machine1)
    (- (machine-carrots machine1) 1)
    (+ (machine-bank machine1) 70)
    0
    (- (machine-tray machine1) 70))
   machine1))

(check-expect (dispense-carr (make-machine 30 50 0 50 0))
              (make-machine 30 50 0 50 0))
(check-expect (dispense-carr (make-machine 30 50 0 100 0))
              (make-machine 30 49 70 0 30))

;;release-tray : Machine -> Machine
;;GIVEN: takes a machine state before the request for
;;       releasing the inserted money
;;RETURNS: the machine state after releasing the inserted money
;;EXAMPLES:
;;  (release-tray (make-machine 30 50 0 200 0)) => (make-machine 30 50 0 0 200)
;;  (release-tray (make-machine 30 50 100 0 0)) => (make-machine 30 50 100 0 0)
;;STRATEGY: Structure Decomposition

(define (release-tray machine1)
  (make-machine (machine-chocolates machine1)
                (machine-carrots machine1)
                (machine-bank machine1)
                0
                (machine-tray machine1)))

(check-expect (release-tray (make-machine 30 50 0 200 0))
              (make-machine 30 50 0 0 200))
(check-expect (release-tray (make-machine 30 50 100 0 0)) 
              (make-machine 30 50 100 0 0))

;;TESTS:
(begin-for-test
  (check-equal? (initial-machine 30 50) (make-machine 30 50 0 0 0)
                "the machine is not being initialised properly")
  
  (check-equal? (initial-machine 0 0) (make-machine 0 0 0 0 0)
                "the machine is not being initialised properly")
  
  (check-equal? (machine-next-state (machine-next-state 
                                     (initial-machine 40 60) 200) "chocolate")
                (make-machine 39 60 175 0 25)
                "chocolate is not being dispensed as expected")
  
  (check-equal? (machine-next-state (machine-next-state 
                                     (initial-machine 40 60) 100) "carrots")
                (make-machine 40 59 70 0 30)
                "carrots' package is not being dispensed as expected")
  
  (check-equal? (machine-next-state (machine-next-state 
                                     (initial-machine 40 60) 100) "chocolate")
                (make-machine 40 60 0 100 0)
                "Insufficient funds (<$1.75) should not dispense anything")
  
  (check-equal? (machine-next-state (machine-next-state 
                                     (initial-machine 40 60) 50) "carrots")
                (make-machine 40 60 0 50 0)
                "Insufficient funds (<$0.70) should not dispense anything")
  
  (check-equal? (machine-next-state (machine-next-state 
                                     (initial-machine 30 20) 300) "release")
                (make-machine 30 20 0 0 300)
                "the money in the tray should be emptied 
                 when customer requests to release"))