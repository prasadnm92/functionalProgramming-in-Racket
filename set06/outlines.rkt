;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;;Purpose Statement 
;A program called "outlines.rkt" that converts from the nested 
;representation to the flat representation.

(require "extras.rkt")
(require rackunit)

;;Providing Functions
(provide nested-rep?
         nested-to-flat)

;;CONSTANTS
(define INITIAL-CONTEXT 0)
(define INCR 1)
(define ZERO 0)

;;--------------------DATA DEFINITIONS-------------------------------------

;An Sexp is one of the following
; -- a String
; -- a NonNegInt
; -- a ListOf<Sexp>
;;TEMPLATE:
;sexp-fn : Sexp -> ??
#;(define (sexp-fn sexp)
    (cond
      [(string? sexp) ...]
      [(integer? sexp) ...]
      [(list? sexp) ...]))

; A ListOf<Sexp> (LOSE) is one of
; -- empty
; -- (cons Sexp ListOf<Sexp>)
;;TEMPLATE:
;lose-fn : LOSE -> ??
#;(define (lose-fn lose)
    (cond
      [(empty? lose) ...]
      [else (... (sexp-fn (first lose))
                 (lose-fn (rest lose)))]))

;;------------------------------------------------------------------------

;;NestedRep and FlatRep are subsets of Sexp.

;;Data Definitions for NestedRep:

;A NestedRep is a ListOf<NrSexp>
;A ListOf<NrSexp> is one of
; -- empty
; -- (cons NrSexp ListOf<NrSexp>)
;;TEMPLATE:
;nr-list-fn : ListOf<NrSexp> -> ??
#;(define (nr-list-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (nr-sexp-fn (first lst))
                 (nr-list-fn (rest lst)))]))

;An NrSexp is a (cons String ListOf<NrSexp>)
;;INTERPRETATION:
; the string is a title of a section
; the ListOf<NrSexp> is a list of subsections, each of type NestedRep
;;TEMPLATE:
;nr-sexp-fn : NrSexp -> ??
#;(define (nr-sexp-fn nr-sexp)
    (...
     (... (first nr-sexp))
     (nr-list-fn (rest nr-sexp))))

;;------------------------------------------------------------------------

;;Data Definitions for FlatRep:

;A FlatRep is a ListOf<FrSexp>
;A ListOf<FrSexp> is one of
; -- empty
; -- (cons FrSexp ListOf<FrSexp>)
;;TEMPLATE:
;fr-list-fn : ListOf<FrSexp> -> ??
#;(define (fr-list-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (fr-sexp-fn (first lst))
                 (fr-list-fn (rest lst)))]))

;An FrSexp is a (cons NELO<NonNegInt> Title)
;;INTERPRETATION:
; the NELO<NonNegInt> is a list of non-negative integers that describe the
;     level at which a section occurs.
; the Tile is a title of a section.
;;TEMPLATE:
;fr-sexp-fn : FrSexp -> ??
#;(define (fr-sexp-fn fr-sexp)
    (...
     (list-fn (first fr-sexp))
     (title-fn (second fr-sexp))))

;A Title is a (cons String empty)
;;INTERPRETATION:
; A tile is a string in a list describing the title of section
;   in a flat representation
;;TEMPLATE:
;title-fn : Title ->
#;(define (title-fn t)
    (... (first t)))

;; A NELO<NonNegInt> (non-empty ListOf<NonNegInt>) is one of
;   -- (cons NonNegInt empty)
;   -- (cons NonNegInt NELO<NonNegInt>)
;;TEMPLATE:
;nelo-fn : NELO<NonNegInt> -> ??
#;(define (nelo-fn ne-lon)
    (cons
     [(empty? (rest ne-lon)) ...]
     [else (... (first ne-lon)
                (nelo-fn (rest ne-lon)))]))


;A MaybeSexp is one of
; -- false
; -- String
; -- NrSexp
;;TEMPLATE
;maybesexp-fn : MaybeSexp -> ??
#;(define (maybesexp-fn msexp)
    (cond
      [(false? msexp) ...]
      [(string? msexp) ...]
      [else (... (nr-sexp-fn msexp))]))

;;--------------------------------------------------------------------------
;Examples for Testing Purposes:

(define input (list (list "The first section"
                          (list "A subsection with no subsections")
                          (list "Another subsection"
                                (list "This is a subsection of 1.2")
                                (list "This is another subsection of 1.2"))
                          (list "The last subsection of 1"))
                    (list "Another section"
                          (list "More stuff")
                          (list "Still more stuff"))))

(define input2 (list (list "The first section" )
                     (list "The second section")))

(define input3 empty)

(define output '(((1) "The first section")
                 ((1 1) "A subsection with no subsections")
                 ((1 2) "Another subsection")
                 ((1 2 1) "This is a subsection of 1.2")
                 ((1 2 2) "This is another subsection of 1.2")
                 ((1 3) "The last subsection of 1")
                 ((2) "Another section")
                 ((2 1) "More stuff")
                 ((2 2) "Still more stuff")))

(define output1 '(((1) "The first section")
                  ((1 1) "A subsection with no subsections")
                  ((1 2) "Another subsection")
                  ((1 2 1) "This is a subsection of 1.2")
                  ((1 2 2) "This is another subsection of 1.2")
                  ((1 3) "The last subsection of 1")))


(define output2 '())

(define sexp1 "random")
(define sexp2 0)
(define sexp3 (list "string1" "string2" "string3"))
(define sexp4 (list "string1" 13 "string2" 23))
(define sexp5 (list (list "Title" (list "Sub-section"))))
(define sexp6 (list "string1" (list "string2" (list 12 23))))
(define sexp7 (list (list "title1")
                    (list #|No Title|#(list "sub1")
                          (list "sub2"))))

;;---------------------END OF DATA DEFINITIONS-----------------------------

;nested-to-flat : NestedRep -> FlatRep
;GIVEN: the representation of an outline as a nested list
;RETURNS: the flat representation of the outline
;EXAMPLE: Refer test cases
;STRATEGY: Function Composition
(define (nested-to-flat nr)
  (nested-to-flat-from nr empty INITIAL-CONTEXT))

;nested-to-flat-from : NestedRep ListOf<NonNegInt> NonNegInt -> FlatRep
;GIVEN: the representation of an outline as a nested list,a list
;        of section indices and a NonNegative Integer that describes the
;        current section being seen.
;WHERE: 
; -- ListOf<NonNegInt> is a sublist of NELO<NonNegInt> that describes the
;    section index last seen.
; -- NonNegInt is a number that represents the index to be added to the 
;    current section being seen.
;RETURNS: the flat representation of the outline
;EXAMPLE:(nested-to-flat-from input2 (list 1) 0) =>
;         (((1 1) "The first section")) 
;STRATEGY: Structural Decomposition on nr : NestedRep
(define (nested-to-flat-from nr num-lst n)
  (cond
    [(empty? nr) empty]
    [else (append (nr-to-fr (first nr) num-lst n)
                  (nested-to-flat-from (rest nr) num-lst (+ INCR n)))]))

;nr-to-fr : NrSexp ListOf<NonNegInt> NonNegInt -> ListOf<FrSexp>
;GIVEN: A section, a list of section indices and a NonNegative 
;       Integer that describes the current section being seen.
;WHERE:
; -- ListOf<NonNegInt> is a sublist of NELO<NonNegInt> that describes the
;    section index last seen.
; -- NonNegInt is a number that represents the index to be added to the 
;    current section being seen.
;RETURNS: A List of FrSexp obatined from the given NrSexp.
;EXAMPLE: (nr-to-fr (first input2) empty 0) =>
;         (((1) "The first section"))
;STRATEGY: Structural Decomposition on outline : NrSexp
(define (nr-to-fr outline num-lst n)
  (cons
   (make-fr-sexp (first outline) num-lst n)
   (nested-to-flat-from (rest outline)
                        (append num-lst (list (+ INCR n))) INITIAL-CONTEXT)))

;make-fr-sexp : String ListOf<NonNegInt> NonNegInt -> FrSexp
;GIVEN: A Section Title, a list of section indices and a NonNegative 
;       Integer that describes the current section being seen.
;WHERE:
; -- ListOf<NonNegInt> is a sublist of NELO<NonNegInt> that describes the
;    section index last seen.
; -- NonNegInt is a number 
;RETURNS: A FrSexp from the given section title and index.
;EXAMPLE:
;    (make-fr-sexp "The first section" empty 0) => ((1) "The first section")
;STRATEGY: Function Composition
(define (make-fr-sexp title num-lst n)
  (cons (append num-lst (list (+ INCR n))) 
        (list title)))


;Test Cases:
(begin-for-test
  (check-equal? (nested-to-flat input) output 
                "Failed to convert a NestedRep expression to 
                 FlatRep expression"))
;;-------------------------------------------------------------------------

;nested-rep? : Sexp -> Boolean
;GIVEN: an Sexp
;RETURNS: true iff it is the nested representation of some outline.
;EXAMPLES: Refer Test Cases
;STRATEGY: Structural Decomposition on sexp : Sexp
(define (nested-rep? sexp)
  (cond
    [(string? sexp) false]
    [(integer? sexp) false]
    [else (check-nested-rep? sexp)]))

;check-nested-rep? : ListOf<Sexp> -> Boolean
;GIVEN: an NestedRep 'sexp'
;RETURNS: true iff it is the nested representation of some outline
;EXAMPLE: (check-nested-rep? sexp5) => true
;STRATEGY: HOFC
(define (check-nested-rep? sections)
  (andmap check-section sections))

;check-section : ListOf<Sexp> -> Boolean
;GIVEN: an NrSexp 'section'
;RETURNS: true iff it is the nested representation of some outline
;EXAMPLE:(check-section sexp5) => true
;STRATEGY: Structural Decomposition on section : ListOf<Sexp>
(define (check-section section)
  (cond
    [(empty? section) false]
    [else (and (string? (list-first section))
               (check-nested-rep? (rest section)))]))

;list-first : Any -> MaybeSexp
;GIVEN: Takes value of any type.
;RETURNS: it returns the first element if given value is a list, else false.
;EXAMPLES:
;    (list-first (list "title1")) => "title1"
;    (list-first "title1") => false
;STRATEGY: Function Composition
(define (list-first lst)
  (if (list? lst)
      (first lst)
      false))


;Test Cases:
(begin-for-test
  (check-equal? (nested-rep? (first input)) false 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? input) true 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? empty) true 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? output) false 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? "string") false 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? sexp7) false 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? 0) false 
                "Failed to check for a given Sexp to be 
                 a NestedRep expression")
  
  (check-equal? (nested-rep? '(() () () ("random"))) false ""))