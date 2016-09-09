;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname obstacles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Purpose Statement:
; The program, 'obstacles.rkt', uses BFS(Breadth first search) algorithm to
;traverse the positions on a chess board. The function 'obstacle?' picks the
;first position from the given list of blocks and traverses all the blocks
;that are reachable from that position. If all the blocks are reachable 
;then it returns true, if not false. The 'blocks-to-obstacle' also works in a 
;similar way as 'obstacle?', just that when there are a set of unreachable 
;block left then it returns a list of these traversed blocks and runs again 
;with untraversed blocks, thus giving a list of obstacles.

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")

(provide position-set-equal?
         obstacle?
         blocks-to-obstacles)

;;CONSTANTS
(define ONE 1)

;----------------------------DATA DEFINTIONS------------------------------------
;;A Position(pos) is a (list PosInt PosInt)
;; INTERPRETATION:
;; (x y) represents a position at the mentioned x and y co-ordinates.
;; WHERE: a Position is always a non-empty list with only two positive integer 
;;        values in it.
;; TEMPLATE:
;; pos-fn : Position -> ??
#;(define (pos-fn p)
    (... (first p)
         (second p)))
;; EXAMPLES:(list 1 2), (list 1 3)

;;A PositionSet is one of
;; -- empty                             Interp: an empty list
;; -- (cons Position PositionSet)       Interp: a non-empty list of Positions
;; WHERE: a PositionSet is a list of positions without duplication.
;; TEMPLATE:
;; pos-set-fn : PositionSet -> ??
#;(define (pos-set-fn ps)
    (cond
      [(empty? ps) ...]
      [else (... (pos-fn (first ps))
                 (pos-set-fn (rest ps)))]))
;; EXAMPLE:(list (list 1 2) (list 2 3))

;;A PositionSetSet is one of
;; --empty                              Interp: an empty list
;; --(cons PositionSet PositionSetSet)  Interp: a non-empty list of PositionSets
;; WHERE: No two PositionSets denote the same set of Positions.
;; TEMPLATE:
;; posset-set-fn : PositionSetSet -> ??
#;(define (posset-set-fn pss)
    (cond
      [(empty? pss) ...]
      [else (... (pos-set-fn (first pss))
                 (posset-set-fn (rest pss)))]))
;; EXAMPLE: (list (list (list 1 2) (list 2 6) (list 4 8))
;;                (list (list 5 5)))

;;A SetOf<X> is a ListOf<X>
;; --empty                              Interp: an empty set
;; --(cons X SetOf<X>)                  Interp: a non-empty set of any 'X'
;; WHERE: there are no duplications of any X.
;; TEMPLATE:
;; set-fn : SetOf<X> -> ??
#;(define (set-fn s)
    (cond
      [(empty? s) ...]
      [else (... (first s)
                 (set-fn (rest s)))]))
;; EXAMPLES: (list 1 2 3 4), (list (list 1 2) (list 2 3))

;;A NonNegPosition is a (list NonNegInt NonNegInt)
;; INTERPRETATION:
;; (x y) represents a position at the mentioned x and y co-ordinates.
;; WHERE: a Position is always a non-empty list with only two non-negative
;;         integer values in it.
;; TEMPLATE:
;; nnpos-fn : NonNegPosition -> ??
#;(define (pos-fn p)
    (... (first p)
         (second p)))
;; EXAMPLES:(list 0 2), (list 2 0), (list 0 0)

;;A NonNegPositionSet is one of
;; -- empty                                   Interp: an empty list
;; -- (cons NonNegPosition NonNegPositionSet) Interp: a non-empty list of 
;                                                     NonNegPositions
;; WHERE: a NonNegPositionSet is a list of positions without duplication.
;; TEMPLATE:
;; nnpos-set-fn : NonNegPositionSet -> ??
#;(define (pos-set-fn ps)
    (cond
      [(empty? ps) ...]
      [else (... (pos-fn (first ps))
                 (pos-set-fn (rest ps)))]))
;; EXAMPLE:(list (list 0 2) (list 2 0) (list 0 0))


;--------------------END OF DATA DEFINITIONS------------------------------------

;;Examples for Testing Purposes:

(define pset1 '((2 3) (5 6)))
(define pset2 '((1 2) (2 1)))
(define pset3 '((10 11) (11 10)))
(define pset4 '((11 10) (10 11)))
(define pset5 '((1 1) (2 2) (3 3) (1 3) (3 1)))
(define pset6 '((1 2) (1 3) (2 3) (3 2) (3 4) (4 1) (4 4)))
(define obstacle6 '(((4 4)) ((1 3)) ((1 2) (2 3) (3 4) (3 2) (4 1))))


;position-set-equal? : PositionSet PositionSet -> Boolean
;GIVEN: two PositionSets.
;RETURNS: true iff they denote the same set of positions.
;EXAMPLES:
;   See tests below
;STRATEGY: Function Composition.
(define (position-set-equal? pset1 pset2)
  (set-equal? pset1 pset2))

;TESTS:
(begin-for-test
  (check-equal? (position-set-equal? pset1 pset2)
                false
                "Failed to test the given position set for inequality, when the
                 given position set are not equal")
  (check-equal? (position-set-equal? pset3 pset4)
                true
                "Failed to test the given position set for equality, when the
                 given position set are equal"))

;-------------------------------------------------------------------------------

;obstacle? : PositionSet -> Boolean
;GIVEN: a PositionSet.
;WHERE: all the positions in the given PositionSet are occupied and
;       all other positions are vacant.
;RETURNS: true iff the set of positions would be an obstacle.
;EXAMPLES:
;   See tests below
;STRATEGY:Structural Decomposition on pset : PositionSet.
(define (obstacle? pset)
  (cond
    [(empty? pset) false]
    [else (if (empty? (rest pset)) true
              (position-set-equal? pset (adjacent-set pset)))]))

;adjacent-set : PositionSet -> PositionSet
;GIVEN: a PositionSet.
;RETURNS: a PositionSet of all the positions reachable from the first position
;         in the given PositionSet.
;EXAMPLES:
;      (adjacent-set '((1 1) (2 2) (3 3) (1 3) (3 1)))
;                  =>  '((1 1) (2 2) (3 3) (1 3) (3 1)) 
;      (adjacent-set '((1 2) (1 3) (2 3) (3 2) (3 4) (4 1) (4 4)))
;                  => '((1 2) (1 3) (2 3) (3 2)(4 1))
;STRATEGY: Function Composition
(define (adjacent-set pset)
  (adjacent-set-from (list (first pset)) pset))

;adjacent-set-from : PositionSet PositionSet -> PositionSet
;GIVEN: 'pset', a set of positions seen so far and 'original-pset', a set
;       positions still to be traversed.
;RETURNS: all the set of position that can be traversed from the given set of
;         seen positions.
;EXAMPLES:
;     (adjacent-set-from '((1 1)) '((1 1) (2 2) (3 3) (1 3) (3 1)))
;                  =>  '((1 1) (2 2) (3 3) (1 3) (3 1)) 
;     (adjacent-set-from  '((4 4)) '((1 2) (1 3) (2 3) (3 2) (3 4) (4 1) (4 4)))
;                  => '((4 4))
;STRATEGY: General Recursion
;TERMINATION ARGUMENT: when there are no remaining blocks to traverse.
;HALTING MEASURE: The number of blocks in common between 'original-pset' and 
;                 all adjacent blocks of traversed blocks in previous recursion.

(define (adjacent-set-from pset original-pset)
  (local
    ((define new-blocks-to-traverse
       (intersection (all-adjacent pset) original-pset))
     (define all-traversed-blocks
       (set-union pset new-blocks-to-traverse))
     (define to-be-traversed
       (set-diff original-pset all-traversed-blocks)))
    (cond
      [(empty? new-blocks-to-traverse) pset]
      [else (adjacent-set-from all-traversed-blocks to-be-traversed)])))

;TESTS:
(begin-for-test
  (check-equal? (obstacle? '((1 1) (3 3) (1 3) (3 1))) false
                "A PositionSet should not be an obstacle when there is any two 
                 positions in the set that are not linked by a chain of adjacent
                 blocks in the set")
  (check-equal? (obstacle? '((1 1) (2 2) (3 3) (1 3) (3 1))) true
                "A PositionSet is an obstacle when all the positions are linked
                 by a chain of adjacent blocks")
  (check-equal? (obstacle? '((1 2) (2 3) (3 4) (4 3) (3 2) (1 4) (3 3))) false
                "A PositionSet with atleast one non-adjacent block is not an 
                 obstacle")
  (check-equal? (obstacle? empty) false
                "An empty PositionSet is not an obstacle")
  (check-equal? (obstacle? '((3 4))) true
                "A PositionSet with a single block is an obstacle")
  (check-equal? (obstacle? '((1 2) (2 1) (3 4) (4 3))) false
                "A PositionSet with two disjoint sets that are linked by 
                 themselves is still not an obstacle")
  (check-equal? (obstacle? '((1 2) (2 1) (2 3) (3 2) (3 4) (4 3))) true
                "A closed circuit of blocks is also a valid obstacle")
  (check-equal? (obstacle? '((1 4) (2 4) (3 3) (4 3))) false
                "Two blocks occuring in the same line are not adjacent, and
                 hence they should not form an obstacle"))

;-------------------------------------------------------------------------------

;blocks-to-obstacles : PositionSet -> PositionSetSet
;GIVEN: the set of occupied positions on some chessboard.
;RETURNS: the set of obstacles on that chessboard.
;EXAMPLES:
;   see tests below
;STRATEGY: Structural Decomposition on pset : PositionSet
(define (blocks-to-obstacles pset)
  (cond
    [(empty? pset) empty]
    [else (blocks-to-obstacles-from empty pset)]))

;blocks-to-obstacles-from : PositionSetSet PositionSet -> PositionSetSet
;GIVEN: 'obstacles', a PositionSetSet of all the obstacles traversed so far and
;       'pset', a PositionSet of all the positions yet to to be traversed.
;RETURNS: a PositionSetSet of the obstacle traversed now and all the obstacles
;         in the given PositionSetSet.
;EXAMPLES:
;   see tests below
;STRATEGY: General Recursion
;TERMINATION ARGUMENT: when there are no remaining blocks to traverse.
;HALTING MEASURE: The remaining number of blocks to traverse.

(define (blocks-to-obstacles-from obstacles pset)
  (local
    ((define new-obstacle
       (adjacent-set pset))
     (define remaining-blocks
       (set-diff pset new-obstacle))
     (define set-of-obstacles
       (cons new-obstacle obstacles)))
    (cond
      [(empty? remaining-blocks) set-of-obstacles]
      [else (blocks-to-obstacles-from set-of-obstacles
                                      remaining-blocks)])))

;TESTS:
(begin-for-test
  (check-equal? (blocks-to-obstacles pset6) obstacle6
                "Any untraversable blocks should be put into a seperate obstacle
                 set")
  (check-equal? (set-equal? (blocks-to-obstacles '((1 1) (1 4) (4 4) (4 1)))
                            '(((1 1)) ((1 4)) ((4 4)) ((4 1))))
                true
                "A PositionSet with each Position non-adjacent to all the other
                Positions should return a PositionSetSet with one Position as
                an obstacle")
  (check-equal? (set-equal? (blocks-to-obstacles-from '(((1 4))) '((4 4)))
                            '(((1 4)) ((4 4))))
                true
                "blocks-to-obstacles-from takes a list of obstacles and should 
                 return a new list of obstacles from the given PositionSet")
  (check-equal? (blocks-to-obstacles empty) empty
                "When there are no blocks in the chess board, the set of 
                 obstacles should be empty"))

;-------------------------------------------------------------------------------

;all-adjacent : PositionSet -> PositionSet
;GIVEN: a set of positions
;RETURNS: a set of all the adjacent positions of each of the position in the
;         given position set.
;EXAMPLE:
;   (all-adjacent '((2 2) (4 3)))
;        => '((3 3) (1 3) (3 1) (1 1) (5 4) (3 4) (5 2) (3 2))
;   (all-adjacent '((2 2) (4 2)))
;        => '((1 3) (1 1) (5 3) (3 3) (5 1) (3 1))
;STRATEGY: Structural Decomposition on pset : PositionSet.
(define (all-adjacent pset)
  (cond
    [(empty? pset) empty]
    [else (set-union (adjacent (first pset))
                     (all-adjacent (rest pset)))]))

;adjacent : Position -> PositionSet
;GIVEN: a position in the chessboard.
;RETURNS: a list of the the positions adjacent to the given position.
;EXAMPLES:
;   (adjacent '(3 3)) => '((4 4) (2 4) (4 2) (2 2))
;   (adjacent '(1 2)) => '((2 3) (2 1))
;STRATEGY: Structural Decomposition on pos : Position
(define (adjacent pos)
  (filter
   (; Position -> Boolean
    ;GIVEN: a position.
    ;RETURNS: true iff the position occurs within the chessboard.
    lambda (pos)
     (and (>= (first pos) ONE) (>= (second pos) ONE)))
   (nonneg-adjacent pos)))

;nonneg-adjacent : Position -> NonNegPositionSet
;GIVEN: a position in the chessboard.
;RETURNS: a list of the the positions adjacent to the given position, which can
;         occur outside the chessboard as well.
;EXAMPLES:
;   (nonneg-adjacent '(3 3)) => '((4 4) (2 4) (4 2) (2 2))
;   (nonneg-adjacent '(1 2)) => '((2 3) (0 3) (2 1) (0 1))
;STRATEGY: Structural Decomposition on pos : Position
(define (nonneg-adjacent pos)
  (list (list (+ (first pos) ONE) (+ (second pos) ONE))
        (list (- (first pos) ONE) (+ (second pos) ONE))
        (list (+ (first pos) ONE) (- (second pos) ONE))
        (list (- (first pos) ONE) (- (second pos) ONE))))

;.......................................................

;intersection : SetOf<X> SetOf<X> -> SetOf<X>
;GIVEN: two sets of any type 'X'.
;RETURNS: the intersection set of the two sets.
;EXAMPLES:
;   (intersection '(1 2 3 4) '(3 6 7 4)) => '(3 4)
;   (intersection '(1 2 3 4) '(5 6 7 8)) => empty
;STRATEGY: HOFC
(define (intersection set1 set2)
  (filter
   (;Any -> Boolean
    ;GIVEN: each value from the set1.
    ;RETURNS: true iff that value is also present in another set, set2, of same
    ;         type as that of set1.
    lambda (x) (my-member? x set2))
   set1))

;set-diff : SetOf<X> SetOf<X> -> SetOf<X>
;GIVEN: two sets of any type 'X'.
;RETURNS: the difference of the two sets, i.e., all the elements in set1 that
;         are not in set2.
;EXAMPLES:
;   (set-diff '(2 37 48 61 23 47 673) '(25 2356 37 47)) => '( 2 48 61 23 673)
;   (set-diff '(1 234 36 45) '(1 234 36 45)) => empty
;STRATEGY: HOFC.
(define (set-diff set1 set2)
  (filter
   (;Any -> Boolean
    ;GIVEN: each value from the set1.
    ;RETURNS: true iff that value is not present in another set, set2, of same
    ;         type as that of set1.
    lambda (x) (not (my-member? x set2)))
   set1))