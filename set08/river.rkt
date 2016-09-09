;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname river) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(provide list-to-pitchers
         pitchers-to-list
         pitchers-after-moves
         make-move
         move-src
         move-tgt
         move?
         make-fill
         fill-pitcher
         fill?
         make-dump
         dump-pitcher
         dump?
         solution)

;;CONSTANTS
(define ZERO 0)
(define ONE 1)
(define TWO 2)

;;============================================================================;;
;;============================================================================;;
;;                              DATA DEFINITIONS                              ;;
;;============================================================================;;
;;============================================================================;;

;;A Pitcher is a (list PosInt PosInt)
;; INTERPRETATION:
;; -- 'capacity' describes the maximum capacity the pitcher can hold
;; -- 'contents' describes the quantity that the pitcher currently holds
;; WHERE: the 'contents' can never be greater than the 'capacity'
;; TEMPLATE:
;; pitcher-fn : Pitcher -> ??
#;(define (pitcher-fn p)
    (... (first p) (second p)))
;; EXAMPLE: (define pitcher1 (list 10 7))

;;A PitchersExternalRep is one of
;; -- (cons Pitcher empty)                   Interp: list contains one Pitcher
;; -- (cons Pitcher PitchersExternalRep)     Interp: list contains one or more
;;                                                   Pitchers
;; TEMPLATE:
;; pit-ext-fn : PitchersExternalRep -> ??
#;(define (pit-ext-fn pe)
    (cond
      [(empty? (rest pe)) ...]
      [else (... (pitcher-fn (first pe))
                 (pit-ext-fn (rest pe)))]))
;; EXAMPLE: (define nelop1 (list (list 10 5) (list 7 2)))

;;==============================================================================

;;InternalPitcher is a (list NonNegInt PosInt PosInt)
;; INTERPRETATION:
;; -- 'capacity' describes the maximum capacity the pitcher can hold
;; -- 'contents' describes the quantity that the pitcher currently holds
;; -- 'index' describes the 'i' for the i-th pitcher in PitchersExternalRep
;; WHERE: the 'contents' can never be greater than the 'capacity'
;; AND: if the InternalPitcher is a river, 
;;      the 'capacity' of river is equal to zero, 'contents' of the river is
;;      sum of all the capacities and 'index' is one more than that of the last
;;      pitcher in a list
;; TEMPLATE:
;; int-pitcher-fn :InternalPitcher -> ??
#;(define (int-pitcher-fn p)
    (... (first p) (second p) (third p)))
;; EXAMPLE: (define pitcher1 (list 8 8 1))
;;          (define river (list 0 16 4))

;;A PitchersInternalRep (state) is one of:
;; -- (cons InternalPitcher empty)                  Interp: list contains one
;;                                                          InternalPitcher
;; -- (cons InternalPitcher PitchersInternalRep)    Interp: list contains one or
;;                                                          more InternalPitcher
;; WHERE: the last element of the PitchersInternalRep is a river
;; TEMPLATE: 
;; pit-int-fn : PitchersInternalRep -> ??
#;(define (pit-int-fn pi)
    (cond
      [(empty? (rest pi)) ...]
      [else (... (int-pitcher-fn (first pi))
                 (pit-int-fn (rest pi)))]))
;; EXAMPLE: (define pit-int1 (list (list 8 8 1) (list 5 0 2) (list 0 13 3)))

;;A ListOf<PitchersInternalRep> is one of
;; -- (cons PitchersInternalRep empty)             Interp:list contains one
;;                                                      <PitchersInternalRep>
;; -- (cons PitchersInternalRep 
;;          ListOf<PitchersInternalRep>)           Interp:list contains one or
;;                                                    more <PitchersInternalRep>
;; TEMPLATE:
;; lst-pit-int-fn : ListOf<PitchersInternalRep> -> ??
#;(define (lst-pit-int-fn lst)
    (cond
      [(empty? (rest lst)) ...]
      [else (... (pit-int-fn (first lst))
                 (lst-pit-int-fn (rest lst)))]))
;; EXAMPLE: (define list-pit-int1 los1) 

;;==============================================================================

(define-struct fill (pitcher))
(define-struct dump (pitcher))
(define-struct move (src tgt))
;;A Move is one of
;; -- Fill is a (make-fill PosInt)
;; -- Dump is a (make-dump PosInt)
;; -- Pour is a (make-move PosInt PosInt)
;; INTERPRETATION:
;; -- (make-fill i) means pour from river to pitcher
;; -- (make-dump i) means pour from pitcher to river
;; -- (make-move i j) means pour from pitcher i to pitcher j
;; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep
;; WHERE: src and tgt are different
;; AND: every move refers only to pitchers that are in the set of pitchers
;; TEMPLATE:
;; move-fn : Move -> ??
#;(define (move-fn m)
    (cond
      [(fill? m) (... (fill-pitcher))]
      [(dump? m) (... (dump-pitcher))]
      [(move? m) (... (move-src m)
                      (move-tgt m))]))
;; EXAMPLE: (make-move 1 2), (make-dump 1), (make-fill 2)

;; ListOf<Move> (LOM)
;; A LOM is one of:
;; -- empty             interp: when the list is empty
;; -- (cons Move LOM)   interp: when the list contains one or more Move
;; WHERE: there are no consecutive duplicate moves
;; TEMPLATE:
;; lom-fn : LOM -> ??
#;(define (lom-fn lom)
    (cond
      [(empty? lom) ...]
      [else (...
             (move-fn (first lom))
             (lom-fn (rest lom)))]))
;; EXAMPLE: (define lom1 (list (list 1 2) (list 2 3)))

;;Maybe<ListOf<Move>> is one of:
;; --false             interp: when there is no possible list of moves
;; --ListOf<Move>      interp: describes a list of moves
;; TEMPLATE:
;; mlom-fn: Maybe<ListOf<Move>> -> ??
#;(define (mlom-fn mlom)
    (cond
      [(false? mlom) ...]
      [else (... (lom-fn mlom))]))
;;EXAMPLE: (define mlom1 false)

;;==============================================================================

;;A ListOf<PosInt> is one of
;; -- empty
;; -- (cons PosInt ListOf<PosInt>
;; TEMPLATE:
;; lst-fn: ListOf<PosInt> -> ??
#;(define (lst-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (first lst)
                 (lst-fn (rest lst)))]))
;; EXAMPLE: empty, (list 1 2 3)

;;A NEListOf<PosInt> is a (cons PosInt ListOf<PosInt>)
;; INTERPRETATION:
;; it is a non-empty list of positive integer values
;; TEMPLATE:
;; ne-lst-fn: NEListOf<PosInt> -> ??
#;(define (ne-lst-fn ne-lst)
    (... (first ne-lst)
         (lst-fn (rest ne-lst))))
;; EXAMPLE: (list 1 2 3 4)

;;==============================================================================



;; EXAMPLES FOR TESTING:

(define init-state (list (list (list 8 8 1) (list 5 0 2) (list 3 0 3))))
(define los1 (list 
              (list (list 8 8 1) (list 5 0 2) (list 3 0 3))
              (list (list 8 5 1) (list 5 0 2) (list 3 3 3))
              (list (list 8 3 1) (list 5 5 2) (list 3 0 3))))

;;------------------------------------------------------------------------------

;;list-to-pitchers : PitchersExternalRep -> PitchersInternalRep
;;GIVEN: a list of pitchers with their capacities and contents
;;RETURNS: a list of pitchers with their capacities, contents and indices along
;;         with an entry for the river
;;EXAMPLES:
;;   See test cases
;;Strategy: HOFC
(define (list-to-pitchers pit-ext-rep)
  (list-to-pitchers-from pit-ext-rep ONE 
                         (foldr + ZERO (map first pit-ext-rep))))

;;TEST CASES:
(begin-for-test
  (check-equal? (list-to-pitchers (list (list 8 8) (list 5 0) (list 3 0))) 
                (list (list 8 8 1) (list 5 0 2) (list 3 0 3) (list 0 16 4))
                "Given a list of Pitchers, should return a list InternalPitchers
                 with an entry for the river as well"))
  


;;list-to-pitchers-from : PitchersExternalRep PosInt PosInt
;;                        -> PitchersInternalRep
;;GIVEN: a list of pitchers with their capacities and contents, the index of the
;;       pitcher which will be seen in this recursion, and the sum of capacities
;;       of all the pitchers in the list
;;RETURNS: a list of pitchers with their capacities, contents and indices
;;         starting from the given index, along with an entry for the river
;;EXAMPLES:
;;   See test cases
;;STRATEGY: SD on pit-ext-rep : PitchersExternalRep
(define (list-to-pitchers-from pit-ext-rep index total-cap)
  (cond
    [(empty? (rest pit-ext-rep))
     (cons (convert-to-int-rep (first pit-ext-rep) index)
           (cons (list ZERO total-cap (+ index ONE)) empty))]
    [else (cons (convert-to-int-rep (first pit-ext-rep) index)
                (list-to-pitchers-from (rest pit-ext-rep) 
                                       (+ index ONE) total-cap))]))

;;Test Cases:
(begin-for-test
  (check-equal? (list-to-pitchers-from (list (list 5 0) (list 3 0)) 2 16) 
                (list (list 5 0 2) (list 3 0 3) (list 0 16 4))
                "Given a subset of list of Pitchers, the current index and the
                 total capacity of all the pitchers in the list, should return a
                 list of InternalPitchers with an entry for the river as well"))


;;convert-to-int-rep: Pitcher PosInt -> InternalPitcher
;;GIVEN: a pitcher with capacity and contents
;;WHERE: index is the index i of the pitcher in the PitchersExternalRep starting
;;       from 1
;;RETURNS: An interal representation of a pitcher
;;EXAMPLE:
;;   (convert-to-int-rep '(8 8) 1) => '(8 8 1)
;;STRATEGY: SD on pitcher : Pitcher
(define (convert-to-int-rep pitcher index)
  (list (first pitcher) (second pitcher) index))

;;------------------------------------------------------------------------------

;;pitchers-after-moves : PitchersInternalRep ListOf<Move> -> PitchersInternalRep
;;GIVEN: An internal representation of a set of pitchers,and a sequence of moves
;;WHERE: every move refers only to pitchers that are in the set of pitchers.
;;RETURNS: the internal representation of the set of pitchers that should
;;         result after executing the given list of moves, in order, on the 
;;         given set of pitchers
;;EXAMPLES:
;;   See test cases
;;STRATEGY: HOFC
(define (pitchers-after-moves pit-int-rep lom)
  (foldl pitcher-after-move pit-int-rep lom))

;; Test Cases:
(begin-for-test 
  (check-equal? 
   (pitchers-after-moves (list (list 8 8 1) (list 5 0 2) (list 3 0 3))
                         (list (make-move 1 2) (make-move 2 3)))
   (list (list 8 3 1) (list 5 2 2) (list 3 3 3))
   "Given a list of pitchers and a list of moves, should return the list of
    oitchers after the moves are executed in sequence"))

;;pitcher-after-move : Move PitchersInternalRep -> PitchersInternalRep
;;GIVEN: a move and a state of the pitchers
;;RETUNRS: a state of the pitchers as it should be after the move is executed
;;EXAMPLES: See test cases
;;STRATEGY: SD on move : Move and src,dest:InternalPitcher
(define (pitcher-after-move move pit-int-rep)
  (local
    ((define internal-move (get-move move (length pit-int-rep)))
     (define src (find-pit (move-src internal-move) pit-int-rep))
     (define dest (find-pit (move-tgt internal-move) pit-int-rep)))
    (sort-index (append (transfer src dest)
                        (filter 
                         ;InternalPitcher -> Boolean
                         ;GIVEN: each pitcher in the given list of pitchers
                         ;RETURNS: true iff it is not one of the source or 
                         ;         target in the current transfer
                         (lambda (each) 
                           (not (member? (third each) 
                                         (list (third src) (third dest)))))
                         pit-int-rep)))))

;;get-move : Move PosInt -> Move
;;GIVEN: a move and the index of the river in the internal representation of
;;       list of pitchers
;;RETURNS: a move with source and target based on given move
;;WHERE: a Fill will return a Move with river as source
;;AND: a Dump will return a Move with river as target
;;EXAMPLES:
;;   (get-move (make-fill 2) 4) => (make-move 4 2)
;;   (get-move (make-dump 2) 4) => (make-move 2 4)
;;   (get-move (make-move 2 3)) => (make-move 2 3)
;;STRATEGY: SD on move : Move
(define (get-move move river-index)
  (cond
    [(fill? move) (make-move river-index (fill-pitcher move))]
    [(dump? move) (make-move (dump-pitcher move) river-index)]
    [(move? move) move]))

;; Test Cases:
(begin-for-test
  (check-equal? (pitcher-after-move  (make-move 1 2) 
                                     (list (list 8 8 1) (list 5 0 2)
                                           (list 3 0 3) (list 0 16 4)))
                (list (list 8 3 1) (list 5 5 2) (list 3 0 3) (list 0 16 4))
                "a move between two pitchers should modify the corrosponding
                 pitchers in the returned list")
  (check-equal? (pitcher-after-move  (make-fill 1) 
                                     (list (list 8 0 1) (list 5 0 2)
                                           (list 3 0 3) (list 0 16 4)))
                (list (list 8 8 1) (list 5 0 2) (list 3 0 3) (list 0 16 4))
                "a move from the river to a pitcher should modify only the 
                 pitcher in the returned list")
  (check-equal? (pitcher-after-move  (make-dump 1) 
                                     (list (list 8 8 1) (list 5 0 2)
                                           (list 3 0 3) (list 0 16 4)))
                (list (list 8 0 1) (list 5 0 2) (list 3 0 3) (list 0 16 4))
                "a move to the river from a pitcher should modify only the 
                 pitcher in the returned list"))

;;find-pit : PosInt PitchersInternalRep -> InternalPitcher
;;GIVEN: src/dest index of a move and the list of pitchers from which a pitcher
;;       needs to be accessed       
;;RETURNS: the pitcher at the given src position
;;EXAMPLES:
;;   See test cases
;;STRATEGY: HOFC
(define (find-pit pos pit-int-rep)
  (first (filter
          ;InternalPitcher -> Boolean
          ;GIVEN: each pitcher in the given list of pitchers
          ;RETURNS: true iff the pitcher's index is same as given index position
          (lambda (each) (equal? (third each) pos))
          pit-int-rep)))

;; Test Cases:
(begin-for-test
  (check-equal? (find-pit (move-tgt (make-move 2 4))
                          (list (list 8 8 1) (list 5 0 2) (list 3 0 3) 
                                (list 0 16 4)))
                (list 0 16 4)
                "find-pit should return pitcher(target) with the given index")
  (check-equal? (find-pit (move-src (make-move 2 3))
                          (list (list 8 8 1) (list 5 0 2) (list 3 0 3)))
                (list 5 0 2)
                "find-pit should return pitcher(source) with the given index"))
;;------------------------------------------------------------------------------

;;pitchers-to-list : PitchersInternalRep -> PitchersExternalRep
;;GIVEN: an internal representation of a set of pitchers
;;RETURNS: a PitchersExternalRep that represents them
;;EXAMPLE:
;;   See test cases
;;STRATEGY: SD on each-int-pit : InternalPitcher
(define (pitchers-to-list pit-int-rep)
  (map
   (;InternalPitcher -> Pitcher
    ;GIVEN: a internal pitcher with capacity, contents and index
    ;RETURNS: a pitcher with its capacity and contents
    lambda (each-int-pit) (list (first each-int-pit) (second each-int-pit)))
   (reverse (rest (reverse pit-int-rep)))))

;;TEST CASES:
(begin-for-test
  (check-equal? (pitchers-to-list (list (list 8 8 1) (list 5 0 2) (list 3 0 3) 
                                        (list 0 16 4)))
                (list (list 8 8) (list 5 0) (list 3 0))
                "Given an internal representation of a list of pitchers, should
                 return the external representation with the indices removed and
                 the entry for river removed"))

;;==============================================================================

;;solution : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;;GIVEN: a list of the capacities of the pitchers and the goal amount
;;RETURNS: a sequence of moves which, when executed from left to right,
;;         results in one pitcher (not necessarily the first pitcher) containing
;;         the goal amount.  Returns false if no such sequence exists.
;;EXAMPLES:
;;   See test cases
;;STRATEGY: HOFC
(define (solution list-of-cap goal)
  (local
    ((define pit-int-rep (make-internal-rep list-of-cap)))
    (if (my-member? goal list-of-cap)
        (list (generate-one-move goal pit-int-rep))
        (if (> goal (foldr max ZERO list-of-cap))
            false
            (solution-from empty (list pit-int-rep) goal empty)))))

;;generate-one-move : PosInt NEListOf<PosInt> -> Move
;;GIVEN: the goal amount and a list of the capacities of pitchers
;;RETURNS: a Fill move to be made to reach the goal amount
;;EXAMPLES:
;;   (generate-one-move 8 (list 8 5 3 8)) => empty
;;   (generate-one-move 3 (list 8 5 3 8)) => (make-move 1 3)
;;STRATEGY: SD on each-pitcher : InternalPitcher
(define (generate-one-move goal pit-int-rep)
  (make-fill (third 
              (first (filter
                      (;InternalPitcher -> Boolean
                       ;GIVEN: a internal pitcher with its capacity, contents 
                       ;       and index
                       ;RETURNS: true iff it's capacity is same as 
                       ;         goal amount
                       lambda (each-pitcher)
                        (equal? goal (first each-pitcher)))
                      pit-int-rep)))))

;;solution-from : ListOf<PitchersInternalRep> ListOf<PitchersInternalRep>
;;                PosInt ListOf<PitchersInternalRep> -> Maybe<ListOf<Move>> 
;;GIVEN: A list of states already visited, a list of states to be traversed,
;;       the goal amount, and a list of states traversed so far
;;WHERE: 1) visited is a list of states already been checked for goal
;;       2) stack holds the current list of states yet to be traversed
;;       3) states-so-far is a sublist of some states-so-far0 above the sublist 
;;       in the whole list
;;RETURNS: a sequence of moves which, when executed from left to right,
;;         results in one pitcher (not necessarily the first pitcher) containing
;;         the goal amount.  Returns false if no such sequence exists
;;EXAMPLES:
;;   See test cases
;;STRATEGY: General Recursion
;;HALTING MEASURE: The number of possible transfers of contents of each pitcher
;;                 in PitchersInternalRep _not_ in visited
(define (solution-from visited stack goal states-so-far)
  (cond
    [(empty? stack) false]
    [else (local
            ((define new-visited (set-union (list (first stack)) visited))
             (define children (build-children (first stack) visited))
             (define new-stack (append children (rest stack)))
             (define new-states-so-far
               (set-union states-so-far (list (first stack)))))
            
            (if (my-member? goal (map second (first stack))) 
                (generate-list-of-move new-states-so-far)
                (solution-from new-visited
                               new-stack
                               goal
                               new-states-so-far)))]))
;;TERMINATION ARGUMENT:
;; At each recursive call a PitchersInternalRep from the stack is retrieved and
;; checked if the goal is present in it. If the goal is reached, the recursion
;; terminates here. Else:
;; The Stack contains the remaining PitchersInternalRep to be visited
;; An empty stack would indicate that no PitchersInternalRep are left to check 
;; and goal is not found.
;; The call terminates here.

;;TEST CASES:
(begin-for-test
  (check-equal? (solution (list 8 5 3) 4) 
                (list (make-fill 1) (make-move 1 2) (make-move 1 3) 
                      (make-move 2 1) (make-dump 1) (make-move 3 1)
                      (make-move 1 2) (make-fill 1) (make-move 1 2)
                      (make-move 1 3) (make-move 2 1) (make-fill 2)
                      (make-dump 3) (make-move 1 3) (make-move 2 1)
                      (make-dump 1) (make-move 2 1) (make-move 3 1)
                      (make-move 1 2) (make-fill 1) (make-move 1 3)
                      (make-move 2 1) (make-move 3 1) (make-move 1 2)
                      (make-dump 1) (make-move 2 1) (make-move 1 3))
                "an achievable goal amount given to a function should return a 
                 list of moves that when executed in sequence will give the goal
                 amount in one of the pitchers")
  (check-equal? (solution (list 8) 8) (list (make-fill 1))
                "a list with single pitcher and same goal amount should return a
                 make-fill to that pitcher")
  (check-equal? (solution (list 8 5) 5) (list (make-fill 2))
                "a list with some pitcher and a goal amount existing in the
                 pitcher list should return a make-fill to that pitcher")
  (check-equal? (solution (list 8 6 4) 5) false
                "a non-achievable goal amount should return a false, i.e., all
                 given pitchers are even and goal is odd")
  (check-equal? (solution (list 8 5 3) 10) false
                "a non-achievable goal amount should return a false, i.e., a
                 goal amount that is greater than all the pitchers")
  (check-equal? (solution-from empty (list (make-internal-rep (list 8 3 5)))
                               4 empty)
                (list (make-fill 1) (make-move 1 2) (make-move 1 3)
                      (make-move 2 1) (make-dump 1) (make-move 3 1)
                      (make-move 1 2) (make-move 1 3) (make-move 2 1)
                      (make-dump 1) (make-move 3 1) (make-move 1 2)
                      (make-fill 1) (make-move 1 2) (make-move 1 3)
                      (make-move 2 1) (make-move 3 1) (make-move 1 2)
                      (make-dump 2) (make-move 1 2) (make-move 1 3))
                "an achievable goal amount given to a function should return a 
                 list of moves that when executed in sequence will give the goal
                 amount in one of the pitchers"))

;;build-children : PitchersInternalRep ListOf<PitchersInternalRep>
;;                                                -> ListOf<PitchersInternalRep>
;;GIVEN: a state of pitchers at some point of time, and a list of states that
;;       have already been visited before reaching the given state of pitchers
;;WHERE:  visited is a list of states already been checked for goal
;;RETURNS: a list of states reachable from the given state of pitchers and that
;;         are not already visited before and no duplicates
;;EXAMPLE:
;;   (build-children '((8 8 1) (5 0 2) (3 0 3) (0 16 4)) empty)
;;    => '(((8 3 1) (5 5 2) (3 0 3) (0 16 4))
;;         ((8 5 1) (5 0 2) (3 3 3) (0 16 4))
;;         ((8 0 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 5 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 3 3) (0 16 4)))
;;STRATEGY: Function Composition
(define (build-children pitchers-state visited)
  (set-diff (build-all-children pitchers-state) 
            (cons pitchers-state visited)))

;;build-all-children: PitchersInternalRep -> ListOf<PitchersInternalRep>
;;GIVEN: a state of pitchers at some point of time
;;RETURNS: a list of states reachable from the given state of pitchers
;;EXAMPLE:
;;   (build-all-children '((8 8 1) (5 0 2) (3 0 3) (0 16 4)))
;;    => '(((8 3 1) (5 5 2) (3 0 3) (0 16 4))
;;         ((8 5 1) (5 0 2) (3 3 3) (0 16 4))
;;         ((8 0 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 5 2) (3 0 3) (0 16 4))
;;         ((8 8 1) (5 0 2) (3 3 3) (0 16 4)))
;;Strategy: HOFC
(define (build-all-children pitchers-state)
  (foldr append empty 
         (map
          (;InternalPitcher -> ListOf<PitchersInternalRep>
           ;GIVEN: each pitcher of the given state of pitchers
           ;RETURNS: all posiible states that can be reached by making transfers
           ;         from the current pitcher
           lambda (each-pitcher) 
            (children-for-each 
             each-pitcher 
             (filter
              (;InternalPitcher -> Boolean
               ;GIVEN: each pitcher of the given state of pitchers
               ;RETURNS: true iff it is not the pitcher from which a transfer
               ;         is being made
               lambda (each) (not (equal? each each-pitcher)))
              pitchers-state)))
          pitchers-state)))


;;children-for-each : InternalPitcher PitchersInternalRep 
;;                    -> ListOf<PitchersInternalRep>
;;GIVEN: a pitcher and a list of all other pitchers a transfer can be made to
;;RETURNS: a list of all the states that are possible to achieve by transfers
;;         from the given pitcher to each pitcher in the given list of pitchers
;;EXAMPLES:
;;   (children-for-each '(8 8 1) '((5 0 2) (3 0 3)))
;;    =>'(((8 3 1) (5 5 2) (3 0 3))
;;        ((8 5 1) (5 0 2) (3 3 3)))
;;STRATEGY: HOFC
(define (children-for-each pit pit-list)
  (map 
   (;InternalPitcher -> PitchersInternalRep
    ;GIVEN: each pitcher that can be a destination in a transfer
    ;RETURNS: a state of pitchers after a transfer has been made
    lambda (each-pit-in-rest) 
     (sort-index 
      (append (transfer pit each-pit-in-rest)
              (filter
               (;InternalPitcher -> Boolean
                ;GIVEN: each pitcher in the list of destination pitchers
                ;RETURNS: true iff the pitcher is not the destination in 
                ;         this transfer
                lambda (each) (not (equal? each each-pit-in-rest)))
               pit-list))))
   pit-list))


;;transfer : InternalPitcher InternalPitcher -> PitchersInternalRep
;;GIVEN: two pitchers, 'pit1' and 'pit2'
;;RETURNS: a list of the two pitchers after a tranfer has been made from pit1 to
;;         pit2
;;EXAMPLE:
;;   (transfer '(8 8 1) '(5 0 2)) => '((8 3 1) (5 5 2))
;;STRATEGY: SD on pit1 and pit2 : InternalPitcher
(define (transfer pit1 pit2)
  (local
    ((define pit-len (length pit1))
     (define tranfer-amount
       (min (second pit1) (if (zero? (first pit2))
                              (second pit1)
                              (- (first pit2) (second pit2))))))
    (list (list (first pit1) 
                (if (zero? (first pit1))
                    (second pit1)
                    (- (second pit1) tranfer-amount)) 
                (third pit1))
          (list (first pit2) 
                (if (zero? (first pit2))
                    (second pit2)
                    (+ (second pit2) tranfer-amount)) 
                (third pit2)))))

;;==============================================================================

;;generate-list-of-move: ListOf<PitchersInternalRep> -> ListOf<Move>
;;GIVEN: a sublist of list of states, in sequence, that occured to achieve some
;;       goal amount
;;RETURNS: a sequence of moves that occured between each pair of consecutive
;;         states in the given list
;;EXAMPLE:
;;   (generate-list-of-move '(((8 0 1) (5 0 2) (3 0 3) (0 16 4))
;;                            ((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;                            ((8 3 1) (5 5 2) (3 0 3) (0 16 4))
;;                            ((8 0 1) (5 5 2) (3 3 3) (0 16 4))
;;                            ((8 5 1) (5 0 2) (3 3 3) (0 16 4))
;;                            ((8 5 1) (5 3 2) (3 0 3) (0 16 4))
;;                            ((8 2 1) (5 3 2) (3 3 3) (0 16 4))))
;;    => '((make-fill 1)
;;         (make-move 1 2)
;;         (make-move 1 3)
;;         (make-move 2 1)
;;         (make-move 3 2)
;;         (make-move 1 3))     
;;STRATEGY: SD on list-of-states : ListOf<PitchersInternalRep>
(define (generate-list-of-move list-of-states)
  (cond
    [(empty? (rest list-of-states)) empty]
    [else (cons (make-this-move (first list-of-states) 
                                (second list-of-states)
                                (length (first list-of-states)))
                (generate-list-of-move (rest list-of-states)))]))


;;make-this-move: PitchersInternalRep PitchersInternalRep PosInt -> Move
;;GIVEN: two consecutive states, 'state1' and 'state2', whose move needs to be
;;       computed, and the number of pitchers in the representation
;;RETURNS: a move that when executed, changes the state of pitchers from
;;         'state1' to 'state2'
;;EXAMPLE:
;;   (make-this-move '(((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;                     ((8 3 1) (5 5 2) (3 0 3) (0 16 4)) 4) => (make-move 1 2)
;;   (make-this-move '(((8 0 1) (5 0 2) (3 0 3) (0 16 4))
;;                     ((8 8 1) (5 0 2) (3 0 3) (0 16 4)) 4) => (make-fill 1)
;;Strategy: Function Composition
(define (make-this-move state1 state2 len)
  (local
    ((define src (get-index state1 state2 >))
     (define dst (get-index state1 state2 <)))
    (if (= src len) (make-fill dst) 
        (if (= dst len) (make-dump src) (make-move src dst)))))


;;get-index: PitchersInternalRep PitchersInternalRep (Number Number -> Boolean)
;;           -> PosInt
;;GIVEN: two states, 's1' and 's2', of pitchers and a function, 'opn', that
;;       returns a boolean value based on the values of the two numbers it takes
;;RETURNS: the index value of a pitcher whose value before a move and after a
;;         move return true for the given funtion, 'opn'
;;EXAMPLE:
;;   (get-index '((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;              '((8 0 1) (5 0 2) (3 0 3) (0 16 4)) <) => 4
;;STRATEGY: SD on s1 : PitchersInternalRep
(define (get-index s1 s2 opn)
  (local
    ((define pitcher1 (first s1))
     (define pitcher2 (first s2)))
    (cond
      [(empty? (rest s1)) (third pitcher2)]
      [else (if (opn (second pitcher1) (second pitcher2))
                (third pitcher1)
                (get-index (rest s1) (rest s2) opn))])))

;;==============================================================================

;;sort-index: PitchersInternalRep -> PitchersInternalRep
;;GIVEN: a list of pitchers
;;RETURNS: a sorted list of pitchers, ordered according to its index
;;EXAMPLE:
;;   (sort-index '((8 4 1) (3 0 3) (0 16 4) (5 4 2)))
;;            => '((8 4 1) (5 4 2) (3 0 3) (0 16 4))
;;STRATEGY: Function Composition
(define (sort-index pit-int-rep)
  (sort pit-int-rep 
        (;InternalPitcher InternalPitcher -> Boolean
         ;GIVEN: two pitchers in the given list of pitchers
         ;RETURNS: true iff the index of first pitcher is less than that of the
         ;         second pitcher
         lambda (pit1 pit2) (< (third pit1) (third pit2)))))

;;==============================================================================

;;make-internal-rep : NEListOf<PosInt> -> PitchersInternalRep
;;GIVEN: a non-empty list of pitcher capacities
;;RETURNS: a list of pitchers with zero contents along with a pitcher entry
;;         for the river at the last index
;;EXAMPLE:
;;   (make-internal-rep (list 8 5 3))
;;    => '((8 0 1) (5 0 2) (3 0 3) (0 16 4))
;;Strategy: Function Composition
(define (make-internal-rep pitchers)
  (append (make-internal-rep-from pitchers ONE)
          (list (list ZERO (foldr + ZERO pitchers) (+ (length pitchers) ONE)))))


;;make-internal-rep-from: NEListOf<PosInt> -> PitchersInternalRep
;;GIVEN: a non-empty list of pitcher capacities
;;RETURNS: a list of pitchers with zero contents
;;EXAMPLE:
;;   (make-internal-rep-from (list 8 5 3) 2) => '((8 0 2) (5 0 3) (3 0 4))
;;STRATEGY: SD on pitchers : NEListOf<PosInt>
(define (make-internal-rep-from pitchers index)
  (cond
    [(empty? (rest pitchers)) (cons (list (first pitchers) ZERO index) empty)]
    [else
     (cons (list (first pitchers) ZERO index)
           (make-internal-rep-from (rest pitchers) (+ index ONE)))]))

;;==============================================================================