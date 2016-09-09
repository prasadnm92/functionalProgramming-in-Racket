;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;;;     State Diagram for (a | b)* (c | d)* e
;;;  ______________________________
;;; |                              |
;;; |                ____          |
;;; |               |    |c,d      |
;;; |--->(Err)<---_(CD)<-|         |
;;;       ^        /\  \           |(any)
;;;  (not |       /     \          |
;;;  a,b,c|   a,b/       \e        |
;;;   d,e)|     /         \        |
;;;       |    /           \       |
;;;       |   /     e      _\/     |
;;;   --->(AB)------------>((EE))--|
;;;        ^ |      
;;;        |_|
;;;        a,b
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 initial-state
 next-state
 accepting-state?
 error-state?)

;;a State is one of:
;; --AB, is the start state that expects either of a, b, c, d or an e
;;   and no other
;; --CD, is a state that expects either of c, d or an e and no other
;; --EE, is the accepting state of the finite state machine
;; --Err, is the error state that is achieved when an unexpected character
;;   is entered

;;initial-state : Number -> State
;;GIVEN: a number
;;RETURNS: a representation of the initial state
;;         of your machine.  The given number is ignored.
;;EXAMPLES:
;; (initial-state 10) => "AB"
;; (initial-state 0) => "AB"

(define (initial-state int)
  "AB")

;KeyEvent is a scalar data
; --any single character key, acts on the state machine
; --any key with more than 1 character, should keep the state unchanged

;;next-state : State KeyEvent -> State
;;GIVEN: a state of the machine and a key event.
;;RETURNS: the state that should follow the given key event.  A key
;;         event that is to be discarded (longer than a single character)
;;         should leave the state unchanged.
;;EXAMPLES:
;; (next-state "AB" "a") => "AB"
;; (next-state "AB" "b") => "AB"
;; (next-state "AB" "c") => "CD"
;; (next-state "AB" "d") => "CD"
;; (next-state "CD" "c") => "CD"
;; (next-state "CD" "d") => "CD"
;; (next-state "CD" "e") => "EE"
;; (next-state "AB" "e") => "EE"
;; (next-state "EE" "a") => "EE"
;; (next-state "AB" "f") => "Err"
;; (next-state "CD" "Key") => "CD"
;;STRATEGY: Used a wrapper function to check for KeyEvents longer than
;;          a single character

(define (next-state state ke)
  (cond
    [(> (string-length ke) 1) state]
    [else (next-state1 state ke)]))

;;next-state1 : State KeyEvent -> State
;;GIVEN: a state of the machine and a key event.
;;RETURNS: the state that should follow the given key event.
;;STRATEGY: Cases on State

(define (next-state1 state ke)
  (cond
    [(string=? state "AB") (state-ab ke)]
    [(string=? state "CD") (state-cd ke)]
    [(string=? state "EE") (state-ee ke)]
    [(string=? state "Err") (state-err ke)]))

;;STRATEGY: Structural Decomposition on KeyEvent

;;state-ab : KeyEvent -> State
;;state-cd : KeyEvent -> State
;;state-ee : KeyEvent -> State
;;state-err : KeyEvent -> State

;;GIVEN: a key event
;;RETURNS: the next state based on the key event on that state,
;;         as described by the machine state diagram
;;STRATEGY: Cases on KeyEvent

(define (state-ab ke)
  (cond
    [(or
      (key=? ke "a")
      (key=? ke "b")) "AB"]
    [(or
      (key=? ke "c")
      (key=? ke "d")) "CD"]
    [(key=? ke "e") "EE"]
    [else "Err"]))

(define (state-cd ke)
  (cond
    [(or
      (key=? ke "c")
      (key=? ke "d")) "CD"]
    [(key=? ke "e") "EE"]
    [else "Err"]))

(define (state-ee ke)
  "Err")

(define (state-err ke)
  "Err")

;;accepting-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true iff the given state is a final (accepting) state
;;EXAMPLES:
;; (accepting-state? "EE") => true
;; (accepting-state? "AB") => false
;; (accepting-state? "Err") => false
;;Strategy: Function Composition

(define (accepting-state? state)
  (string=? state "EE"))
         
;;error-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true iff the string seen so far does not match the specified
;;         regular expression and cannot possibly be extended to do so.
;;EXAMPLES:
;; (error-state? "AB") => false
;; (error-state? "Err") => true
;;Strategy: Function Composition

(define (error-state? state)
  (string=? state "Err"))

;;TESTS
(begin-for-test
  (check-equal? (next-state "AB" "a") "AB"
                "AB---(a)--->AB")
  (check-equal? (next-state "AB" "b") "AB"
                "AB---(b)--->AB")
  (check-equal? (next-state "AB" "c") "CD"
                "AB---(c)--->AB")
  (check-equal? (next-state "AB" "d") "CD"
                "AB---(d)--->AB")
  (check-equal? (next-state "CD" "c") "CD"
                "CD---(c)--->CD")
  (check-equal? (next-state "CD" "d") "CD"
                "CD---(d)--->CD")
  (check-equal? (next-state "CD" "e") "EE"
                "CD---(e)--->EE")
  (check-equal? (next-state "AB" "e") "EE"
                "AB---(e)--->EE")
  (check-equal? (next-state "EE" "a") "Err"
                "EE---(a)--->Err")
  (check-equal? (next-state "AB" "f") "Err"
                "AB---(f)--->Err")
  (check-equal? (next-state "CD" "Key") "CD"
                "CD---(.)+-->CD")
  (check-equal? (next-state "CD" "x") "Err"
                "CD---(x)--->Err")
  (check-equal? (accepting-state?
                 (next-state
                  (next-state
                   (next-state
                    (next-state
                     (next-state
                      (next-state
                       (next-state
                        (initial-state 10)
                        "a") "b") "a") "c") "d") "d") "e"))
                true "abacdde should be an accepting string")
  (check-equal? (accepting-state?
                 (next-state
                  (next-state
                   (next-state
                    (next-state
                     (next-state
                      (next-state
                       (next-state
                        (next-state
                         (initial-state 10)
                         "a") "b") "a") "c") "a") "d") "d") "e"))
                false "abacadde should not be an accepting string")
  (check-equal? (error-state?
                 (next-state
                  (next-state
                   (next-state
                    (next-state
                     (next-state
                      (next-state
                       (next-state
                        (next-state
                         (initial-state 10)
                         "a") "b") "a") "c") "a") "d") "d") "e"))
                true "abacadde generates an error when entering a after c")
  (check-equal? (error-state?
                 (next-state
                  (initial-state 15) "f")) true
                                           "f is an unaccepted string")
  (check-equal? (error-state?
                 (next-state
                  (initial-state 17) "a")) false
                                           "a is a valid string")
  (check-equal? (accepting-state?
                 (next-state
                  (initial-state 23) "e")) true
                                         "e is an accepting state")
  (check-equal? (accepting-state?
                 (next-state
                  (next-state
                   (next-state
                    (next-state
                     (next-state
                      (next-state
                       (next-state
                        (next-state
                         (initial-state 10)
                         "a") "b") "a") "c") "d") "d") "e") "a"))
                false "abacddea should not be an accepting string"))