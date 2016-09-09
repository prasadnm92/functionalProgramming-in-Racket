;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

(provide 
 make-editor
 editor-pre
 editor-post
 edit)

(define-struct editor (pre post))
; an Editor is a (make-editor String String)
; Interpretation:
;  pre is the string of characters that occur before the cursor
;      given by the function (editor-pre)
;  post is the string of characters that occur after the cursor
;      given by the function (editor-post)

;KeyEvent is a scalar data
; --"left" moves the cursor left by one character
; --"right" moves the cursor right by one character
; --"\b", the backspace, removes one character from the left of the cursor
; --any other single character key,puts that character to the left of the cursor

;edit : Editor KeyEvent -> Editor
; GIVEN: takes an Editor and a KeyEvent
; RETURNS: an Editor with the operation specified by the KeyEvent applied on it
;          --"\t", rubout and multi-character strings will not be handled
; EXAMPLES:
;  (edit (make-editor "abd" "e") "left") => (make-editor "ab" "de")
;  (edit (make-editor "ab" "de") "c") => (make-editor "abc" "de")
;  (edit (make-editor "abc" "cde") "\b") => (make-editor "ab" "cde")
; STRATEGY: Cases on KeyEvent

(define (edit ed ke)
  (cond
    [(key=? ke "left") (move-left ed)]
    [(key=? ke "right") (move-right ed)]
    [(key=? ke "\b") (backspace ed)]
    [else (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]))

;;string-first : String -> String
;; GIVEN: a string
;; RETURNS: the first character of the string

(define (string-first str)
  (substring str 0 1))

;;string-last : String -> String
;; GIVEN: a string
;; RETURNS: the last character of the string

(define (string-last str)
  (substring str (- (string-length str) 1)))

;;string-rest : String -> String
;; GIVEN: a string
;; RETURNS: a substring of the input string with the first character removed

(define (string-rest str)
  (substring str 1))

;;string-remove-last : String -> String
;; GIVEN: a string
;; RETURNS: the 

(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

;move-left : Editor -> Editor
; GIVEN: takes an Editor
; RETURNS: an Editor with the cursor moved left by one character
; EXAMPLES:
;  (move-left (make-editor "abd" "e")) => (make-editor "ab" "de")
;  (move-left (make-editor "abd" "")) => (make-editor "ab" "d")
; STRATEGY: Structure Decomposition

(define (move-left ed)
  (cond
    [(string=? "" (editor-pre ed)) ed]
    [else 
     (make-editor
      (string-remove-last (editor-pre ed))
      (string-append (string-last (editor-pre ed)) (editor-post ed)))]))

;TESTS
(check-equal? (move-left (make-editor "hello" " world"))
              (make-editor "hell" "o world"))
(check-equal? (move-left (make-editor "" "hello world"))
              (make-editor "" "hello world"))
(check-equal? (move-left (make-editor "hello world" ""))
              (make-editor "hello worl" "d"))

;move-right : Editor -> Editor
; GIVEN: takes an Editor
; RETURNS: an Editor with the cursor moved right by one character
; EXAMPLES:
;  (move-right (make-editor "abd" "e")) => (make-editor "abde" "")
;  (move-right (make-editor "" "abd")) => (make-editor "a" "bd")
; STRATEGY: Structure Decomposition

(define (move-right ed)
  (cond
    [(string=? "" (editor-post ed)) ed]
    [else
     (make-editor
      (string-append (editor-pre ed) (string-first (editor-post ed)))
      (string-rest (editor-post ed)))]))

;TESTS
(check-equal? (move-right (make-editor "hello" " world"))
              (make-editor "hello " "world"))
(check-equal? (move-right (make-editor "" "hello world"))
              (make-editor "h" "ello world"))
(check-equal? (move-right (make-editor "hello world" ""))
              (make-editor "hello world" ""))

;backspace : Editor -> Editor
; GIVEN: takes an Editor
; RETURNS: an Editor with a character deleted from the left of the cursor
; EXAMPLES:
;  (backspace (make-editor "abc" "cde")) => (make-editor "ab" "cde")
;  (backspace (make-editor "abcc" "")) => (make-editor "abc" "")
;  (backspace (make-editor "" "cde")) => (make-editor "" "cde")
; STRATEGY: Structure Decomposition

(define (backspace ed)
  (cond
    [(string=? "" (editor-pre ed)) ed]
    [else
     (make-editor
      (string-remove-last (editor-pre ed))
      (editor-post ed))]))

;TESTS
(check-equal? (backspace (make-editor "hello" " world"))
              (make-editor "hell" " world"))
(check-equal? (backspace (make-editor "" "hello world"))
              (make-editor "" "hello world"))
(check-equal? (backspace (make-editor "hello world" ""))
              (make-editor "hello worl" ""))

;TESTS:
(begin-for-test
  (check-equal? (make-editor "hello " "world")
                (make-editor "hello " "world")
                "editor is not being created as required")
  
  (check-equal? (edit (make-editor "helo" " world") "left")
                (make-editor "hel" "o world")
                "\"left\" key event should move the cursor left by 1 char")
  
  (check-equal? (edit (edit (make-editor "helo" " world") "left") "l")
                (make-editor "hell" "o world")
                "a single char key event should add
                 that character to the left of cursor")
  
  (check-equal? (edit (edit (edit 
                             (make-editor "helo" " world") "left") "l") "right")
                (make-editor "hello" " world")
                "\"right\" key event should move the cursor right by 1 char")
  
  (check-equal? (edit (make-editor "helloX" " world") "\b")
                (make-editor "hello" " world")
                "\b should delete a single char to the left of cursor"))