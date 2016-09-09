;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pitchers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(provide list-to-pitchers
         pitchers-to-list
         pitchers-after-moves
         solution
         make-move
         move-src
         move-tgt)

;;CONSTANTS:
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
;; -- the first element is 'capacity' which describes the maximum 
;;    capacity the pitcher can hold
;; -- the second element is 'contents' which describes the 
;;    quantity that the pitcher currently holds
;; WHERE: the 'contents' can never be greater than the 'capacity'
;; TEMPLATE:
;; pitcher-fn : Pitcher -> ??
#;(define (pitcher-fn p)
    (... (first p) (second p)))
;; Example: (define pitcher1 (list 10 7))

;;==============================================================================

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
;;Example: (define nelop1 (list (list 10 5) (list 7 2)))

;;==============================================================================

;;InternalPitcher is a (list PosInt PosInt PosInt)
;; INTERPRETATION:
;; -- first element 'capacity' is the maximum capacity of the pitcher
;; -- second element 'contents' is the quantity that the pitcher currently holds
;; -- third element 'index' is 'i' for the i-th pitcher in PitchersExternalRep
;; WHERE: the 'contents' can never be greater than the 'capacity'
;; TEMPLATE:
;; int-pitcher-fn : Pitcher -> ??
#;(define (int-pitcher-fn p)
    (... (first p) (second p) (third p)))
;; Example: (define pitcher1 (list 8 8 1))

;;A PitchersInternalRep (state) is one of:
;; -- (cons InternalPitcher empty)                  Interp: list contains one
;;                                                          InternalPitcher
;; -- (cons InternalPitcher PitchersInternalRep)    Interp: list contains one or
;;                                                          more InternalPitcher
;; TEMPLATE: 
;; pit-int-fn : PitchersInternalRep -> ??
#;(define (pit-int-fn pi)
    (cond
      [(empty? (rest pi)) ...]
      [else (... (int-pitcher-fn (first pi))
                 (pit-int-fn (rest pi)))]))
;; Example: (define pit-int1 (list (list 8 8 1) (list 5 0 2)))

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
;; Example: (define list-pit-int1 los1) 

;;==============================================================================

(define-struct move (src tgt))
;;A Move is a (make-move PosInt PosInt)
;; INTERP: (make-move i j) means pour from pitcher i to pitcher j
;; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep
;; WHERE: src and tgt are different
;; AND every move refers only to pitchers that are in the set of pitchers
;; TEMPLATE:
;; move-fn : Move -> ??
#;(define (move-fn m)
    (... (move-src m) 
         (move-tgt m)))
;; Example: (define m1 (make-move 1 2))

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
;; Example: (define lom1 (list (list 1 2) (list 2 3)))

;;Maybe<ListOf<Move>> is one of:
;; --false             interp: when there is no list of moves
;; --ListOfMove        interp: describes a list of moves
;; TEMPLATE:
;; mlom-fn: Maybe<ListOf<Move>> -> ??
#;(define (mlom-fn mlom)
    (cond
      [(false? mlom) ...]
      [else (... (lom-fn mlom))]))
;;Example: (define mlom1 false)

;;==============================================================================

;; A ListOf<PosInt> is one of:
;; -- empty                        interp: when list is empty
;; -- (cons PosInt ListOf<PosInt>) interp: when list contains one or more PosInt
;; TEMPLATE:
;; lopi-fn : ListOf<PosInt> -> ??
#;(define (lopi-fn lopi)
    (cond
      [(empty? lopi) ...]
      [else (... (... (first lopi))
                 (lopi-fn (rest lopi)))]))
;; Example: (define lopi1 (list 1 2 3))

;; NEListOf<PosInt>1 is one of:
;; -- (cons PosInt empty)            interp: when list contains one PosInt
;; -- (cons PosInt NEListOf<PosInt>1) interp: list contains one or more PosInt
;; TEMPLATE:
;; nelopi1-fn : NEListOf<PosInt>1 -> ??
#;(define (nelopi1-fn nelopi)
    (cond
      [(empty? (rest nelopi)) ...]
      [else (... (... (first nelopi))
                 (nelopi1-fn (rest nelopi)))]))
;; Example: (define nelopi1 (list 2 3 4))

;;A NEListOf<PosInt>2 is a (cons PosInt ListOf<PosInt>)
;; INTERPRETATION:
;; it is a non-empty list of positive integer values
;; TEMPLATE:
;; ne-lst2-fn: NEListOf<PosInt>2 -> ??
#;(define (ne-lst2-fn ne-lst)
    (... (first ne-lst)
         (lst-fn (rest ne-lst))))
;; EXAMPLE: (list 1 2 3 4)

;;==============================================================================

;; Examples for testing:

(define init-state (list (list (list 8 8 1) (list 5 0 2) (list 3 0 3))))
(define los1 (list 
              (list (list 8 8 1) (list 5 0 2) (list 3 0 3))
              (list (list 8 5 1) (list 5 0 2) (list 3 3 3))
              (list (list 8 3 1) (list 5 5 2) (list 3 0 3))))

;;==============================================================================

;;list-to-pitchers : PitchersExternalRep -> PitchersInternalRep
;;Given: A PitchersExternalRep
;;RETURNS:internal representation of PitchersExternalRep i.e PitchersInternalRep
;;Examples:See tests
;;Strategy: Function Composition
(define (list-to-pitchers pit-ext-rep)
  (list-to-pitchers-from pit-ext-rep 1))

;; Test Cases
(begin-for-test
  (check-equal? (list-to-pitchers (list (list 8 8) (list 5 0) (list 3 0))) 
                (list (list 8 8 1) (list 5 0 2) (list 3 0 3))
                "list-to-pitchers with 3 pitchers must return pit-int-rep"))


;;list-to-pitchers-from : PitchersExternalRep PosInt -> PitchersInternalRep
;;Given: A PitchersExternalRep and index starting from 1
;;RETURNS:internal representation of PitchersExternalRep i.e PitchersInternalRep
;;Examples: (list-to-pitchers-from (list (list 8 5) (list 5 0))
;;          => (list (list 8 5 1) (list 5 0 2))
;;Strategy: Function Composition
(define (list-to-pitchers-from pit-ext-rep index)
  (cond
    [(empty? (rest pit-ext-rep))
     (cons (convert-to-int-rep (first pit-ext-rep) index) empty)]
    [else (cons (convert-to-int-rep (first pit-ext-rep) index)
                (list-to-pitchers-from (rest pit-ext-rep) (+ index 1)))]))

;;Test Cases: See tests for list-to-pitchers


;;convert-to-int-rep: Pitcher -> InternalPitcher
;;Given: A Pitcher
;;Returns: An InternalPitcher
;;Examples: (convert-to-int-rep (list 8 5) 1) => (list 8 5 1)
;;Strategy: Structural Decomposition on pitcher:Pitcher
(define (convert-to-int-rep pitcher index)
  (list (first pitcher) (second pitcher) index))

;;Test Cases: See tests for list-to-pitchers

;;==============================================================================

;;pitchers-to-list : PitchersInternalRep -> PitchersExternalRep
;;GIVEN: an internal representation of a set of pitchers
;;RETURNS: a PitchersExternalRep that represents them.
;;Examples: See tests
;;Strategy: SD on each-int-pit : InternalPitcher
(define (pitchers-to-list pit-int-rep)
  (map
   ;InternalPitcher -> Pitcher
   ;Given: An InternalPitcher
   ;Returns: A Picher
   (lambda (each-int-pit) (list (first each-int-pit) (second each-int-pit)))
   pit-int-rep))

;;Test Cases:
(begin-for-test
  (check-equal? (pitchers-to-list (list (list 8 8 1) (list 5 0 2) (list 3 0 3)))
                (list (list 8 8) (list 5 0) (list 3 0))
                "pitchers-to-list with 3 picthers must return pit-ext-rep"))

;;==============================================================================

;;pitchers-after-moves : PitchersInternalRep ListOf<Move> -> PitchersInternalRep
;;GIVEN: An internal representation of a set of pitchers,and a sequence of moves
;;WHERE: every move refers only to pitchers that are in the set of pitchers.
;;RETURNS: the internal representation of the set of pitchers that should
;;         result after executing the given list of moves, in order, on the 
;;         given set of pitchers
;;Examples: See tests
;;Strategy: HOFC
(define (pitchers-after-moves pit-int-rep lom)
  (foldl pitcher-after-move pit-int-rep lom))

;; Test Cases:
(begin-for-test 
  (check-equal? 
   (pitchers-after-moves (list (list 8 8 1) (list 5 0 2) (list 3 0 3))
                         (list (make-move 1 2) (make-move 2 3)))
   (list (list 8 3 1) (list 5 2 2) (list 3 3 3))
   "pitchers-after-moves: must return pit-int-rep after transfer"))

;;pitcher-after-move: PitchersInternalRep Move -> PitchersInternalRep
;;Given: A PitchersInternalRep and a move 
;;Returns: The PitchersInternalRep after the given move
;;Examples: See tests
;;Strategy: SD on src and dest : InternalPitcher
(define (pitcher-after-move move pit-int-rep)
  (local
    ((define src (first (find-src move pit-int-rep)))
     (define dest (first (find-dest move pit-int-rep))))
    (sort-index (append (transfer src dest)
                        (filter 
                         ;InternalPitcher -> PitchersInternalRep
                         ;Given: An InternalPitcher
                         ;Returns: a PitchersInternalRep which contains 
                         ;;        InternalPitchers that are not src or dest
                         (lambda (each) 
                           (not (member? (third each) 
                                         (list (third src) (third dest)))))
                         pit-int-rep)))))

;; Test Cases:
(begin-for-test
  (check-equal? 
   (pitcher-after-move  (make-move 1 2) 
                        (list (list 8 8 1) (list 5 0 2) (list 3 0 3)))
   (list (list 8 3 1) (list 5 5 2) (list 3 0 3))
   "pitcher-after-move: must return state after one move"))

;;==============================================================================

;;solution : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;;GIVEN: a list of the capacities of the pitchers and the goal amount
;;RETURNS: a sequence of moves which, when executed from left to right,
;;         results in one pitcher (not necessarily the first pitcher) containing
;;         the goal amount.  Returns false if no such sequence exists.
;;EXAMPLES: See test cases
;;STRATEGY: Function Composition
(define (solution list-of-cap goal) 
  (local
    ((define pit-int-rep (make-internal-rep list-of-cap)))
    (if (my-member? goal list-of-cap)
        (generate-one-move goal list-of-cap)
        (generate-many-move goal list-of-cap pit-int-rep))))

;; Test Cases:
(begin-for-test
  (check-equal? (solution (list 8 5 3) 4) (list
                                           (make-move 1 2)
                                           (make-move 1 3)
                                           (make-move 2 1)
                                           (make-move 3 2)
                                           (make-move 1 3)
                                           (make-move 3 2)
                                           (make-move 2 1)
                                           (make-move 3 2)
                                           (make-move 1 3))
                "solution: must reurn a list of valid  moves")
  (check-equal? (solution (list 8) 8) empty
                "solution is empty if tgt is first")
  (check-equal? (solution (list 8 5) 5) (list (make-move 1 2))
                "make one-move")
  (check-equal? (solution (list 8 5 3) 3) (list (make-move 1 3))
                "make one-move")
  (check-equal? (solution (list 8 6 4) 5) false 
                "solution does not exist: return false")
  (check-equal? (solution (list 8 5 3) 10) false
                "solution does not exist: return false"))

;;generate-many-move : PosInt NEListOf<PosInt> PitchersInternalRep 
;;                     -> Maybe<ListOf<Move>>
;;GIVEN: goal, non-empty list of capacities,sub-list of PitchersInternalRep
;;WHERE: pit-int-rep is a sublist of some pit-int-rep0 above the sub-list 
;;       in the whole list
;;RETURNS: A List of moves, or false if there is no possible list of moves to 
;;         reach the goal
;;EXAMPLES: (generate-many-move 10 (list 2 5) (list (list (2 0 1) (list (5 0 2))
;;          => false
;;STRATEGY: HOFC
(define (generate-many-move goal list-of-cap pit-int-rep) 
  (if (> goal (foldr max ZERO list-of-cap))
      false
      (solution-from empty (list pit-int-rep) goal empty)))

;;Test Cases: see tests for solution

;;generate-one-move : PosInt NEListOf<PosInt> -> ListOf<Move>
;;GIVEN: the goal amount and a list of the capacities of pitchers
;;RETURNS: a move to be made to reach the goal amount
;;EXAMPLE: (generate-one-move 5 (list 5)) => empty
;;STRATEGY: SD on list-of-cap:NEListOf<PosInt>2
(define (generate-one-move goal list-of-cap)
  (if (equal? goal (first list-of-cap))
      empty
      (list (make-move ONE (get-pos goal (rest list-of-cap) TWO)))))

;;Test Cases: see tests for solution

;;get-pos : PosInt NEListOf<PosInt> PosInt -> PosInt
;;GIVEN: goal, list-of-capacities and an index
;;WHERE: index is the index of an InternalPitcher starting from 2
;;RETURNS: The index of the target pitcher equal to the goal
;;EXAMPLE: (get-pos 5 (list 8 5 3) 2) => 3
;;STRATEGY: SD on list-of-cap:NEListOf<PosInt>2
(define (get-pos goal list-of-cap index)
  (if (equal? goal (first list-of-cap))
      index
      (get-pos goal (rest list-of-cap) (+ index ONE))))

;;Test Cases: see tests for solution

;;solution-from : ListOf<PitchersInternalRep> ListOf<PitchersInternalRep>
;;                PosInt ListOf<PitchersInternalRep> -> Maybe<ListOf<Move>> 
;;Given: A list of states already visited, a list of states to be traversed,
;;       the goal amount, and a list of states traversed so far
;;WHERE: 1) visited is a list of states already been checked for goal
;;       2) stack holds the current list of states yet to be traversed
;;       3) states-so-far is a sublist of some states-so-far0 above the sublist 
;;       in the whole list
;;Returns: a sequence of moves which, when executed from left to right,
;;         results in one pitcher (not necessarily the first pitcher) containing
;;         the goal amount.  Returns false if no such sequence exists
;;Examples: (solution-from empty empty 10 empty) => empty
;;Strategy: General Recursion
;;HALTING MEASURE: The number of possible transfer of contents of each pitcher
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

;;Test Cases: see tests for solution


;;build-children : PitchersInternalRep ListOf<PitchersInternalRep>
;;                                                -> ListOf<PitchersInternalRep>
;;GIVEN: a state of pitchers at some point of time, and a list of states that
;;       have already been visited before reaching the given state of pitchers
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

;;Test Cases: see tests for solution


;;build-all-children PitchersInternalRep -> ListOf<PitchersInternalRep>
;;Given: a state of pitchers at some point of time
;;Returns: a list of PitchersInternalRep which is the children of the given
;;         PitchersInternalRep
;;Examples:  (build-all-children '((8 8 1) (5 0 2) (3 0 3) (0 16 4)))
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
           ;RETURNS: all possible states that can be reached by making transfers
           ;         from the current pitcher
           lambda (each-pitcher)
            (children-for-each 
             each-pitcher 
             (filter
              (;InternalPitcher -> Boolean
               ;GIVEN: each pitcher in the given state of pitchers
               ;RETURNS: true iff it is not the pitcher from which a transfer
               ;         is being made
               lambda (each)
                (not (equal? each each-pitcher)))
              pitchers-state)))
          pitchers-state)))

;;Test Cases: see tests for solution

;;children-for-each : InternalPitcher PitchersInternalRep -> 
;;                                                  ListOf<PitchersInternalRep>
;;GIVEN: an internalpitcher and a list of other pitchers
;;       to which a transfer can be made
;;RETURNS: a list of all the states that are possible after transfers
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
                lambda (each)
                 (not (equal? each each-pit-in-rest)))
               pit-list))))
   pit-list))

;;Test Cases: see tests for solution

;;transfer : InternalPitcher InternalPitcher -> PitchersInternalRep
;;GIVEN: two pitchers, 'pit1' and 'pit2'
;;RETURNS: a list of the two pitchers after a tranfer has been made from pit1 to
;;         pit2 by a possible transfer amount
;;EXAMPLE: (transfer '(8 8 1) '(5 0 2)) => '((8 3 1) (5 5 2))
;;STRATEGY: Structural Decomposition on pit1 and pit2: InternalPitcher
(define (transfer pit1 pit2)
  (local
    ((define tranfer-amount
       (min (second pit1) (- (first pit2) (second pit2)))))
    (list (list (first pit1) (- (second pit1) tranfer-amount) (third pit1))
          (list (first pit2) (+ (second pit2) tranfer-amount) (third pit2)))))

;;Test Cases: see tests for solution


;;generate-list-of-move: ListOf<PitchersInternalRep> -> ListOf<Move>
;;GIVEN: a list of all the states, in sequence, that occured to achieve some
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
;; => (list (make-move 4 1) (make-move 1 2) (make-move 1 3) (make-move 2 1)
;;          (make-move 3 2) (make-move 1 3))   
;;STRATEGY: Structural Decomposition on 
;;                             list-of-states : ListOf<PitchersInternalRep>
(define (generate-list-of-move list-of-states)
  (cond
    [(empty? (rest list-of-states)) empty]
    [else (cons (make-this-move (first list-of-states) (second list-of-states))
                (generate-list-of-move (rest list-of-states)))]))

;;Test Cases: see tests for solution


;;make-this-move: PitchersInternalRep PitchersInternalRep -> Move
;;GIVEN: two consecutive states, 'state1' and 'state2', whose move needs to be
;;       computed
;;RETURNS: a move that when executed, changes the state of pitchers from
;;         'state1' to 'state2'
;;Examples: (make-this-move (list (list 8 8 1) (list 5 0 2))
;;                          (list (list 8 3 1) (list 5 5 2)))
;;          => (make-move 1 2)
;;Strategy: Function Composition
(define (make-this-move state1 state2)
  (make-move (get-index state1 state2 >) (get-index state1 state2 <)))

;;Test Cases: see tests for solution


;;get-index: PitchersInternalRep PitchersInternalRep (Number Number -> Boolean)
;;           -> PosInt
;;GIVEN: two states, 's1' and 's2', of pitchers and a function, 'opn', that
;;       returns a boolean value based on the values of the two numbers it takes
;;RETURNS: the index value of a pitcher whose value before a move and after a
;;         move is true for the given funtion, 'opn'
;;EXAMPLE:
;;   (get-index '((8 8 1) (5 0 2) (3 0 3) (0 16 4))
;;              '((8 0 1) (5 0 2) (3 0 3) (0 16 4)) <) => 4
;;STRATEGY: Structural Decomposition on s1 : PitchersInternalRep
(define (get-index s1 s2 opn)
  (local
    ((define pitcher1 (first s1))
     (define pitcher2 (first s2)))
    (cond
      [(empty? (rest s1)) (if (opn (second pitcher1) (second pitcher2))
                              (third pitcher1)
                              (third pitcher2))]
      [else (if (opn (second pitcher1) (second pitcher2))
                (third pitcher1)
                (get-index (rest s1) (rest s2) opn))])))


;; Test Cases:
(begin-for-test
  (check-equal?
   (get-index (list (list 3 3 1))
              (list (list 3 0 1)) <) 1)
  (check-equal?
   (get-index (list (list 8 5 1) (list 5 0 2) (list 3 3 3))
              (list (list 8 8 1) (list 5 0 2) (list 3 0 3)) >) 3))

;;sort-index: PitchersInternalRep -> PitchersInternalRep
;;GIVEN: a list of pitchers
;;RETURNS: a sorted list of pitchers, in ascending order of its index
;;EXAMPLE:
;;   (sort-index '((8 4 1) (3 0 3) (0 16 4) (5 4 2)))
;;            => '((8 4 1) (5 4 2) (3 0 3) (0 16 4))
;;STRATEGY: Function Composition
(define (sort-index pit-int-rep)
  (sort pit-int-rep 
        (;InternalPitcher InternalPitcher -> Boolean
         ;GIVEN: any two pitchers in the given list of pitchers
         ;RETURNS: true iff the index of first pitcher is less than that of the
         ;         second pitcher
         lambda (pit1 pit2) (< (third pit1) (third pit2)))))

;;Test Cases: see tests for solution

;;make-internal-rep : NEListOf<PosInt> -> PitchersInternalRep
;;GIVEN: a non-empty list of pitcher capacities
;;RETURNS: PitchersInternalRep after converting the given list of capacities
;;EXAMPLE:
;;   (make-internal-rep (list 8 5 3))
;;    =>(list (list 8 8 1) (list 5 0 2) (list 3 0 3))
;;Strategy: Structural Decomposition on pitchers: NEListOf<PosInt>2
(define (make-internal-rep pitchers)
  (cons (list (first pitchers) (first pitchers) ONE)
        (make-initial-rep-from (rest pitchers) TWO)))

;;Test Cases: see tests for solution

;;make-initial-rep-from: ListOf<PosInt> -> PitchersInternalRep
;;GIVEN: a non-empty list of pitcher capacities
;;RETURNS: sub-list of rest of PitchersInternalRep
;;EXAMPLE:
;;   (make-initial-rep-from (list 8 5 3)) => '((8 0 1) (5 0 2) (3 0 3))
;;STRATEGY: Structural Decomposition on pitchers: ListOf<PosInt>
(define (make-initial-rep-from pitchers index)
  (cond
    [(empty? pitchers) pitchers]
    [else
     (cons (list (first pitchers) ZERO index)
           (make-initial-rep-from (rest pitchers) (+ index ONE)))]))

;;Test Cases: see tests for solution

;;==============================================================================

;;find-src: Move PitchersInternalRep -> InternalPitcher
;; Given: A Move and a PitchersInternalRep
;; Returns: The source InternalPitcher among the PitchersInternalRep 
;;          derived from the move
;; Examples: (find-src (list 2 3) (list (list 8 8 1) (list 5 0 2) (list 3 0 3)))
;;           => (list 5 0 2)
;; Strategy: HOFC
(define (find-src move pit-int-rep)
  (filter
   ;InternalPitcher -> Boolean
   ;Given: An InternalPitcher
   ;Returns: true iff the index of the InternalPitcher is same as that of src
   (lambda (each) (equal? (third each) (move-src move)))
   pit-int-rep))

;; Test Cases:
(begin-for-test
  (check-equal? 
   (find-src (make-move 2 3) (list (list 8 8 1) (list 5 0 2) (list 3 0 3)))
   (list (list 5 0 2))
   "find-src ust return the source InternalPitcher"))

;;find-dest: Move PitchersInternalRep -> InternalPitcher
;; Given: A Move and a PitchersInternalRep
;; Returns: The target InternalPitcher among the PitchersInternalRep 
;;          derived from the move
;; Examples: (find-dest (list 2 3) (list(list 8 8 1) (list 5 0 2) (list 3 0 3)))
;;           => (list 3 0 3)
;; Strategy: HOFC
(define (find-dest move pit-int-rep)
  (filter
   ;;InternalPitcher -> Boolean
   ;Given: An InternalPitcher
   ;Returns: true iff the index of the InternalPitcher is same as that of dest
   (lambda (each) (equal? (third each) (move-tgt move)))
   pit-int-rep))

;; Test Cases:
(begin-for-test
  (check-equal? 
   (find-dest (make-move 2 3) (list (list 8 8 1) (list 5 0 2) (list 3 0 3)))
   (list (list 3 0 3))
   "find-dest ust return the tgt InternalPitcher"))