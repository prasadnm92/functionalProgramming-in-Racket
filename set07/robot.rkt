;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Purpose Statement:
;The program, 'robot.rkt', uses BFS(Breadth first search) algorithm to
;traverse the positions on a chess board. The function 'path' takes a start and
;end position and a list of blocks and returns a path for robot to reach the
;end position from the given start position, avoiding the set of blocks. If no
;such path exists, then returns false. The function 'path-from' uses BFS to
;traverse through the chessboard to find a path. It uses a data structure called
;'DirList' which keep tracks of sequence of directions, when we traverse from a 
;position to its adjacent positions. Thus, when end position is reached this
;list of directions is extracted to output the 'Plan' of path.

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(provide path)

;;CONSTANTS
(define ZERO 0)
(define ONE 1)
(define BUFFER 3)
(define EAST "east")
(define WEST "west")
(define NORTH "north")
(define SOUTH "south")

;;---------------------------DATA DEFINITIONS-----------------------------------

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

;...........................................

;;A Direction is one of
;; -- "north"          Interp: represents upward direction.
;; -- "east"           Interp: represents rightward direction.
;; -- "south"          Interp: represents downward direction.
;; -- "west"           Interp: represents leftward direction.
;; TEMPLATE:
;; dir-fn : Direction -> ??
#;(define (dir-fn d)
    (cond
      [(string=? "north") ...]
      [(string=? "east") ...]
      [(string=? "south") ...]
      [(string=? "west") ...]))
;; EXAMPLES: "north", "east"

;;A ListOf<Direction> is one of
;; -- empty
;; -- (cons Direction ListOf<Direction>)
;; TEMPLATE:
;; d-lst-fn : ListOf<Direction> -> ??
#;(define (d-lst-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (dir-fn (first lst))
                 (d-lst-fn (rest lst)))]))
;; EXAMPLE: (list "north" "north" "east" "south" "south")

;...........................................

;;A DirList is a (cons Position ListOf<Direction>)
;; INTERPRETATION:
;; -- 'pos' is the position of a certain location on the chessboard
;; -- 'dir-list' is a list of directions which describes the series of direction
;;    that the robot follows to arrive at the corrosponding position from an
;;    arbitrary start position.
;; TEMPLATE:
;; dirlist-fn : DirList -> ??
#;(define (dirlist-fn dl)
    (... (pos-fn (first dl))
         (d-lst-fn (rest dl))))
;; EXAMPLE: (list (list 1 3) "south" "south" "east" "east")

;;A ListOf<DirList> is one of
;; -- empty
;; -- (cons DirList ListOf<DirList>)
;; TEMPLATE:
;; lst-dirlist-fn : ListOf<DirList> -> ??
#;(define (lst-dirlist-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (dirlist-fn (first lst))
                 (lst-dirlist-fn (rest lst)))]))
;; EXAMPLE: (list (list (list 3 3) "south" "south" "east" "east")
;;                (list (list 3 4) "south" "south" "east" "east" "south"))

;...........................................

;;A Move is a (list Direction PosInt)
;; INTERPRETATION:
;; a move of the specified number of steps in the indicated direction.
;; TEMPLATE:
;; move-fn : Move -> ??
#;(define (move-fn m)
    (... (first m) (second m)))
;; EXAMPLES: (list "north" 5), (list "west" 50)

;;A ListOf<Move> is one of
;; -- empty
;; -- (cons Move ListOf<Move>)
;; TEMPLATE:
;; lst-move-fn : ListOf<Move> -> ??
#;(define (lst-move-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (move-fn (first lst))
                 (lst-move-fn (rest lst)))]))
;; EXAMPLE:(list (list "north" 5) (list "north" 50))

;...........................................

;;A Plan is a ListOf<Move>
;; WHERE: the list does not contain two consecutive moves in the same direction.
;; TEMPLATE:
;; plan-fn : Plan -> ??
#;(define (plan-fn plan)
    (cond
      [(empty? plan) ...]
      [else (... (move-fn (first plan))
                 (plan-fn (rest plan)))]))
;; EXAMPLE:(list (list "north" 5) (list "west" 50))

;;A MaybePlan is one of
;; -- false            Interp: describes that it is not a Plan.
;; -- Plan             Interp: describes that it is a Plan.
;; TEMPLATE:
;; maybeplan-fn : MaybePlan -> ??
#;(define (maybeplan-fn mp)
    (cond
      [(false? mp) ...]
      [else (... (plan-fn mp))]))
;; EXAMPLES: false, (list (list "east" 100))

;-----------------------------------TEST DATA-----------------------------------
;Examples for testing purposes:

(define blocks1 '((5 1) (5 2) (4 3) (6 5) (6 6) (7 6) (8 6) (9 6)
                        (5 7) (4 8) (6 9) (7 9)))
(define blocks2 '((2 2) (3 2) (4 2) (4 3) (4 4) (3 4) (2 4) (2 3)))

(define blocks3 '((4 1) (4 2) (4 3) (4 4) (3 4) (2 4) (1 4)))

(define blocks4 '((7 1) (7 2) (7 3) (7 4) (7 5) (7 6) (7 7)
                        (1 7) (2 7) (3 7) (4 7) (5 7) (6 7)))

(define blocks5 '((2 2) (3 2) (4 2) (5 2) (6 2)
                        (6 3) (6 4) (6 5) (6 6) 
                        (2 6) (3 6) (4 6) (5 6) 
                        (2 3) (2 4) (2 5)))

(define blocks6 '((2 1) (3 2) (2 3) (1 5) (1 6) (3 4) (3 5)
                        (5 5) (5 6) (1 7) (2 7) (3 7) (4 7) (5 7) (6 7)))

;---------------------------FUNCTION DEFINITIONS--------------------------------

;;path : Position Position ListOf<Position> -> Maybe<Plan>
;;GIVEN: 1. the starting position of the robot,
;;       2. the target position that robot is supposed to reach
;;       3. A list of the blocks on the board
;;RETURNS: a plan that, when executed, will take the robot from the starting 
;;         position to the target position without passing over any of the
;;         blocks, or false if no such sequence of moves exists.
;;EXAMPLES:
;;   see test cases
;;STRATEGY: Function Composition
(define (path start end blocks)
  (local
    ((define max-x
       (foldr max (max (first start) (first end)) (map first blocks)))
     (define max-y
       (foldr max (max (second start) (second end)) (map second blocks))))
    
    (if (equal? start end) empty
        (if (my-member? end blocks) false
            (path-from start end blocks max-x max-y (list (list start))
                       (all-adjacent (list(list start)) blocks max-x max-y))))))
;;TESTS:
(begin-for-test
  (check-equal? (path (list 2 2) (list 6 6) blocks6)
                '(("west" 1) ("south" 2) ("east" 1) ("south" 2) ("east" 2)
                             ("north" 2) ("east" 2) ("south" 2))
                "The robot failed to reach its target position for the given
                 source, target and list of obstacles")
  (check-equal? (path (list 1 2) (list 1 2) empty)
                empty
                "The source and target position are same for the robot
                 hence the 'Plan' should be empty")
  (check-equal? (path (list 1 2) (list 2 1) blocks6)
                false
                "The target position is part of the obstacles and hence no 
                 path exists"))

;;path-from : Position Position ListOf<Position> PosInt PosInt 
;;            ListOf<DirList> ListOf<DirList> -> Maybe<Plan>
;;GIVEN: 's' and 'e', the start and end positions
;;       'b', list of occupied positions on the chessboard
;;       'max-x' and 'max-y', the maximum limits of the x and y coordinates 
;;        amongst the given start, end and occupied positions
;;       'visited',set of DirList, that describes all the positions that have
;;        already been traversed
;;       'last-adjacent',set of DirList, that describes the adjacent positions
;;        obtained from the previous recursion.
;;RETURNS: a plan that, when executed, will take the robot from the starting 
;;         position to the target position without passing over any of the
;;         blocks, or false if no such sequence of moves exists.
;;EXAMPLE:
;;   see test cases
;;STRATEGY: General Recursion
;;TERMINATING ARGUMENT: the 'successors', set of adjacent positions of traversed
;;                      positions, either becomes empty or the end position 
;;                      has been reached
;;HALTING MEASURE: the no. of positions in the chessboard that are not yet 
;;                 visited
(define (path-from s e b max-x max-y visited last-adjacent)
  (local
    ((define successors
       (dir-list-set-diff (all-adjacent last-adjacent b max-x max-y) visited))
     (define new-visited
       (dir-list-set-union last-adjacent visited)))
    (cond
      [(empty? successors) false]
      [(my-member? e (map first successors)) (generate-plan e successors)]
      [else (path-from s e b max-x max-y new-visited successors)])))

;;Tests
(begin-for-test
  (check-equal? (path-from (list 1 2) (list 6 6) blocks6 10 10 
                           '(((1 2))) 
                           '(((2 2) "east") 
                             ((1 3) "south") 
                             ((1 1) "north")))
                '(("south" 2) ("east" 1) ("south" 2) ("east" 2) ("north" 2) 
                              ("east" 2) ("south" 2))
                "The path-from function should start from the start position and
                 a list of its adjacent positions to traverse the chessboard")
  (check-equal? (path-from (list 1 2) (list 6 6) '((2 2) (1 1) (1 3)) 10 10
                           '(((1 2))) empty)
                false
                "The succesors for the given start positions are empty
                 as all the adjacent positions are part of obstacles
                 hence there are no possible paths"))

;;generate-plan : Position ListOf<DirList> -> Plan
;;GIVEN: end position, and list of DirList traversed to reach the end position
;;RETURNS:  a plan that, when executed, will take the robot from the starting 
;;         position to the target position without passing over any of the
;;         blocks.
;;EXAMPLES:
;;   see test cases
;;STRATEGY: Function Composition
(define (generate-plan end adj-list)
  (generate-plan-from (string-path end adj-list) ZERO empty))

;;TEST:
(begin-for-test 
  (check-equal? (generate-plan (list 1 3) '(((1 3) "south")))
                '(("south" 1))
                "generate-plan function should provide the plan which the 
                 robot takes to reach its target position from source"))


;;string-path : Position ListOf<DirList> -> ListOf<Direction>
;;GIVEN: end position and list of DirList traversed to reach the end position
;;RETURNS: list of directions that when executed will take the robot from start
;;         to end position.
;;EXAMPLE:
;;    see test cases
;;STRTEGY : HOFC
(define (string-path end adj-list)
  (rest (first (filter
                (;DirList -> Boolean
                 ;GIVEN: each DirList of the given ListOf<DirList>
                 ;RETURNS: DirList corresponding to the end position
                 lambda (each-pos)
                  (equal? (first each-pos) end))
                adj-list))))
;;TEST:
(begin-for-test
  (check-equal? (string-path '(2 6) 
                             '(((5 5) "east" "south" "south" "east" "east")
                               ((4 6) "east" "south" "south" "south" "east")
                               ((3 7) "east" "south" "south" "south" "south")
                               ((2 6) "west" "south" "south" "south" "east")
                               ((1 7) "west" "south" "south" "south" "south")
                               ((5 1) "north" "north" "east" "east" "east")))
                '("west" "south" "south" "south" "east")
                "string-path function should give list of directions for the
                 sequence of trversed positions from start to end"))

;;generate-plan-from : ListOf<Direction> NonNegInt Plan -> Plan
;;GIVEN: list of direction 'str-list', a NonNegInt 'ctxt' and a Plan 'plan-list'
;;WHERE: 'str-list' is a list of directions yet to be processed into a plan
;;AND: 'ctxt' is a count of no. of same directions seen till previous recursion
;;AND: 'plan-list' is a list of moves generated from all the directions seen
;;      so far.
;;RETURNS: a Plan generated from the set of directions seen till the previous
;;         recursion
;;EXAMPLES:
;;   (generate-plan-from '("south" "east") 0 empty)
;;       => '(("south" 1) ("east" 1))
;;   (generate-plan-from '("north" "north" "east") 3 '(("west" 4)))
;;       => '(("west" 4) ("north" 5) ("east" 1))
;;STRATEGY : General Recursion
;;TERMINATION ARGUMENT: when there are no more Directions that have to be 
;;                      processed into a ListOf<Move>
;;HALTING MEASURE: The no. of Directions to be processed decreases by one on
;;                 every recursion, till it becomes 0
(define (generate-plan-from str-list ctxt plan-list)
  (local
    ((define new-plan-list
       (append plan-list (list (list (first str-list) (+ ctxt ONE))))))
    (cond
      [(empty? (rest str-list)) new-plan-list]
      [(equal? (second str-list) (first str-list)) 
       (generate-plan-from (rest str-list) (+ ctxt ONE) plan-list)]
      [else (generate-plan-from (rest str-list) ZERO new-plan-list)])))

;-------------------------------------------------------------------------------

;all-adjacent: ListOf<DirList> ListOf<Position> PosInt PosInt -> ListOf<DirList>
;GIVEN: 'pset', a list of positions along with their directions; 'blocks', a 
;       list of Positions that are occupied on the chessboard; 'max-x' and 
;       'max-y', the maximum limits of the x and y coordinates amongst the
;       given start, end and ocuupied positions.
;RETURNS: a set of all the adjacent positions of each of the position in the
;         given DirList.
;EXAMPLE:
;   see tests below
;STRATEGY: Structural Decomposition on pset : PositionSet
(define (all-adjacent pset blocks max-x max-y)
  (cond
    [(empty? pset) empty]
    [else (dir-list-set-union (adjacent (first pset) blocks max-x max-y)
                              (all-adjacent (rest pset) blocks max-x max-y))]))

;;;TEST:
(begin-for-test
  (check-equal? (all-adjacent '(((1 2))) blocks6 10 10)
                '(((2 2) "east") ((1 3) "south") ((1 1) "north"))
                "all-adjacent function should give the successors list along 
                 with the direction of the move"))

;;adjacent : DirList ListOf<Position> PosInt PosInt -> ListOf<DirList>
;;GIVEN: a position 'pos' in the chessboard with its corrosponding list of 
;;       Directions that describe the order of traversal from some position p0
;;       list of Positions 'blocks' that are occupied on the chessboard,
;;       'max-x' and 'max-y', the maximum limits of the x and y coordinates 
;;       amongst the given start, end and ocuupied positions.       
;RETURNS: a list of the the positions adjacent to the given position with each 
;;        of its list of Directions updated to the order of traversal from the
;;        given position to its adjacent position.
;;EXAMPLE:
;;   (adjacent '((2 2)) blocks6 9 10) => '(((1 2) west) ((2 1) "north"))
;;STRATEGY: Structural Decomposition on pos : DirList
;;                                  adj-pos : DirList
(define (adjacent pos blocks x-limit y-limit)
  (local
    ((define position
       (first pos)))
    (filter
     (; DirList -> Boolean
      ;GIVEN: a position with its corrosponding list of directons.
      ;RETURNS: true iff the position occurs within the chessboard and within
      ;         the maximum x and y limits and that is not a block.
      lambda (adj-pos)
       (filter-valid-adjacents (first adj-pos) blocks x-limit y-limit))
     (list (cons (list (+ (first position) ONE) (second position)) 
                 (append (rest pos) (list EAST)))
           (cons (list (- (first position) ONE) (second position)) 
                 (append (rest pos) (list WEST)))
           (cons (list (first position) (+ (second position) ONE)) 
                 (append (rest pos) (list SOUTH)))
           (cons (list (first position) (- (second position) ONE)) 
                 (append (rest pos) (list NORTH)))))))

;;filter-valid-adjacents : DirList ListOf<Position> PosInt PosInt -> Boolean
;;GIVEN: 'adj-pos', a position with its corrosponding list of directons
;;       'blocks', a list positions that are occupied on the chessboard
;;       'x-limit' and 'y-limit', the maximum x and y positions that occurs in
;;       the given start, end and blocks.
;;RETURNS: true iff the given occurs within the chessboard and within
;;         the maximum x and y limits and that is not a block.
;;EXAMPLES:
;;    (filter-valid-adjacents '((1 2) "north" "west") '((1 1) (1 2)) 10 10)
;;                              => false
;;    (filter-valid-adjacents '((1 2) "north" "west") '((1 1) (3 3)) 10 10)
;;                              => true
;;STRATEGY: Structural Decomposition on pos : Position
(define (filter-valid-adjacents pos blocks x-limit y-limit)
  (and (>= (first pos) ONE) 
       (>= (second pos) ONE)
       (<= (first pos) (+ x-limit BUFFER)) 
       (<= (second pos) (+ y-limit BUFFER))
       (not (my-member? pos blocks))))

;;------------------------------------------------------------------------------

;;dir-list-set-union : ListOf<DirList> ListOf<DirList> -> ListOf<DirList>
;;GIVEN: two lists of positions with their corrosponding directions of traveral.
;;RETURNS: the union of the given two lists, with no duplicates
;;EXAMPLE:
;;   (dir-list-set-union '(((1 3) "south" "south" "east" "west")
;;                         ((3 5) "north" "west" "east"))
;;                       '(((1 3) "south" "south" "east")
;;                        ((3 4) "north" "west" "west")))
;;            => '(((3 4) "north" "west" "west")
;;                 ((1 3) "south" "south" "east")
;;                 ((3 4) "north" "west" "west"))
;;STRATEGY: HOFC
(define (dir-list-set-union set1 set2)
  (foldr dir-list-set-cons set2 set1))

;;dir-list-set-cons : DirList ListOf<DirList> -> ListOf<DirList>
;;GIVEN: an element (a position with its corrosponding list of directions of
;;       traversal) and a list of that type of elements.
;;RETURNS: a list like the given, with the given element added, if its not
;;         already present.
;;EXAMPLE:
;;   (dir-list-set-cons '((1 3) "south" "south" "east" "west")
;;                      '(((1 3) "south" "south" "east")
;;                        ((3 4) "north" "west" "west")))
;;           => '(((1 3) "south" "south" "east")
;;                ((3 4) "north" "west" "west"))
;;STRATEGY: Structural Decomposition on x : DirList
(define (dir-list-set-cons x set1)
  (if (my-member? (first x) (map first set1))
      set1
      (cons x set1)))

;;dir-list-set-diff : ListOf<DirList> ListOf<DirList> -> ListOf<DirList>
;;GIVEN: two lists of positions with their corresponding directions of traveral.
;;RETURNS: the set of elements in set1 that do not occur in set2.
;;EXAMPLE:
;;   (dir-list-set-diff '(((1 3) "south" "south" "east")
;;                        ((3 4) "north" "west" "west"))
;;                      '(((1 3) "south" "south" "east" "west")
;;                        ((3 5) "north" "west" "east")))
;;            => '(((3 4) "north" "west" "west"))
;;STRATEGY: Structural Decomposition on x : DirList
(define (dir-list-set-diff set1 set2)
  (filter
   (;DirList -> Boolean
    ;GIVEN: each DirList from the set1.
    ;RETURNS: true iff that DirList is not present in another set, set2, of same
    ;         type as that of set1.
    lambda (x) (not (my-member? (first x) (map first set2))))
   set1))