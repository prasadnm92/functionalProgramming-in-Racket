#lang racket
;;PURPOSE STATEMENT:
;; This program simulates a toy factory. A toy factory has a target that is
;; draggable. The target is used to place square toys on the canvas using the
;; "s" key stroke. Once created, the toys can be dragged around the canvas. When
;; a toy intersects with another toy, they become buddies.
;; Buddies move togther. When a toy is selected, all its buddies also turn red
;; and if the toy is dragged, its buddies also move by the same distance.
;; Once two toys become buddies, they stay buddies to eternity.

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/universe)   
(require 2htdp/image) 

(provide World%
         SquareToy%
         make-world
         run
         StatefulWorld<%>
         StatefulToy<%>)

;; CONSTANTS:

(define ZERO 0)
(define ONE 1)
(define FIVE 5)
(define OUTLINE "outline")
;............................................
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-X-CENTER (/ CANVAS-WIDTH 2))
(define CANVAS-Y-CENTER (/ CANVAS-HEIGHT 2))
;............................................
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define NON-RESPONSIVE-MOUSE-EVENT "move")

(define S "s")
(define NON-RESPONSIVE-KEY-EVENT "c")
;............................................
;;CONSTANTS FOR WORLD:
    (define TARGET-RADIUS 10)
    (define UNSELECTED-TARGET-COLOR "black")
    (define UNSELECTED-TARGET-IMG 
      (circle TARGET-RADIUS OUTLINE UNSELECTED-TARGET-COLOR))
    (define SELECTED-TARGET-COLOR "orange")
    (define SELECTED-TARGET-IMG 
      (circle TARGET-RADIUS OUTLINE SELECTED-TARGET-COLOR))
;............................................
;;CONSTANTS for toy:
    (define SQ-TOY-LEN 30)
    (define HALF-SQ-TOY-LEN (/ SQ-TOY-LEN 2))
    (define UNSELECTED-SQ-TOY-COLOR "green")
    (define UNSELECTED-SQ-TOY-IMG 
      (square SQ-TOY-LEN OUTLINE UNSELECTED-SQ-TOY-COLOR))
    (define SELECTED-SQ-TOY-COLOR "red")
    (define SELECTED-SQ-TOY-IMG 
      (square SQ-TOY-LEN OUTLINE SELECTED-SQ-TOY-COLOR))

;===============================================================================
;===============================================================================

;; DATA DEFINITIONS:

;A ColorString is one of:
; -- "black"        interp: indicates color of target when unselected
; -- "orange"       interp: indicates color of target when selected
; -- "green"        interp: indicates color of square toy when unselected
; -- "red"          interp: indicates color of square toy when either the toy
;                           or its buddy is selected
; TEMPLATE:
; col-fn : ColorString -> ??
#;(define (col-fn col)
    (cond
      [(string=? col "black") ...]
      [(string=? col "orange") ...]
      [(string=? col "green") ...]
      [(string=? col "red") ...]))

;A ListOfStatefulToy<%> is one of:
; -- empty                                      interp: an empty list of toys
; -- (cons StatefulToy<%> ListOfStatefulToy<%>) interp: a non-empty list of toys
; TEMPLATE:
; lst-fn : ListOfStatefulToy<%> -> ??
#;(define (lst-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (... (first lst))
                 (lst-fn (rest lst)))]))

;===============================================================================
;===============================================================================

;;Interfaces:

; StatefulWorld<%>
(define StatefulWorld<%>
  (interface ()
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this StatefulWorld<%> to the state that it should
    ;; be in after a tick.
    on-tick                             
    
    ;; Integer Integer MouseEvent -> Void
    ;; GIVEN: x,y mouse coordinates and a mouse event
    ;; EFFECT: updates this StatefulWorld<%> to the state that it 
    ;; should be in after the given MouseEvent
    on-mouse
    
    ;; KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this StatefulWorld<%> to the state that it 
    ;; should be in after the given KeyEvent
    on-key
    
    ;; -> Scene
    ;; GIVEN: no arguments
    ;; RETURNS: a Scene depicting this StatefulWorld<%> on it.
    on-draw 
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: Whether the target is selected or not?
    target-selected?
    
    ;; -> ColorString
    ;; GIVEN: no arguments
    ;; RETURNS: color of the target
    target-color
    
    ;; -> ListOfStatefulToy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the list of toys.
    get-toys
    
    ))

; StatefulToy<%>
(define StatefulToy<%> 
  (interface ()
    
    ;; Integer Integer MouseEvent -> Void
    ;; EFFECT: updates this StatefulToy<%> to the 
    ;;         state that it should be in after the given MouseEvent
    on-mouse                       
    
    ;; Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a Scene like the given one, but with this StatefulToy<%> drawn
    ;; on it.
    add-to-scene
    
    ;; -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the specified coordinate of this StatefulToy<%>.
    toy-x
    toy-y
    
    ;; -> ColorString
    ;; GIVEN: no arguments
    ;; RETURNS: the current color of this StatefulToy<%>
    toy-color
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: Whether this StatefulToy<%> is selected or not?
    toy-selected?
    
    ))

;===============================================================================
;===============================================================================

;; Classes:

;;World%  
;; A World is a
;; (new World% [x PosInt]            
;;             [y PosInt]             
;;             [x-click NonNegInt]     
;;             [y-click NonNegInt]      
;;             [selected? Boolean]
;;             [toys ListOfStatefulToy<%>])    
;; Interpretation: represents a world, containing a target and some toys.
(define World%               
  (class* object% (StatefulWorld<%>)         
    (init-field x y)               ; the x & y coordinates of the centre of 
    ;                                the target
    (init-field x-click y-click)   ; the x & y coordinates of the mouse pointer
    ;                                on the last mouse event
    (init-field selected?)         ; true iff the target is selected
    (init-field toys)              ; a ListOfStatefulToy<%>
    
    ;;Example: 
    
    (super-new)
    
    ;; on-tick : -> Void
    ;; EFFECT: updates this StatefulWorld<%> to the state that it should be
    ;;         in after a tick.
    ;; Examples: See tests
    (define/public (on-tick)
      this)
    
    ;; on-mouse : Integer Integer MouseEvent -> Void
    ;; GIVEN: the location of the mouse pointer and a mouse event
    ;; EFFECT: updates this world to as it should be after the given event
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: Cases on mev : MouseEvent
    (define/public (on-mouse mx my mev)
      (cond
        [(mouse=? mev BUTTON-DOWN) 
         (send this world-after-button-down mx my mev)]
        [(mouse=? mev DRAG)
         (send this world-after-drag mx my mev)]
        [(mouse=? mev BUTTON-UP)
         (send this world-after-button-up mx my mev)]
        [else this]))
    
    ;; world-after-button-down : Integer Integer MouseEvent -> Void
    ;; GIVEN: coordinates of the mouse button-down event and the event
    ;; EFFECT: Selects the target iff the event occured inside the target, and
    ;;         each of the toys are updated as it it should be after the event
    ;; Example:
    ;;  A World% with no toys and an unselected target at (200,250) will be
    ;;  selected after a button-down at (210,250)
    ;; STRATEGY: function composition
    (define/public (world-after-button-down mx my mev)
      (if (send this click-in-target? mx my)
          (send this world-after-mouse-event x y mx my true mev)
          (send this world-after-mouse-event x y mx my selected? mev)))
    
    ;; click-in-target? : Integer Integer -> Boolean
    ;; GIVEN: coordinates of the mouse button-down event
    ;; RETURNS: true iff the mouse button-down event occured within the target
    ;; Example:
    ;;  click-in-target? will return true for a World% with target at (200,250)
    ;;  and mouse click at (210,250)
    ;; STRATEGY: function composition
    (define/public (click-in-target? mx my)
      (<= (+ (sqr (- mx x)) (sqr (- my y)))
          (sqr TARGET-RADIUS)))
    
    ;; world-after-button-up : Integer Integer MouseEvent -> Void
    ;; GIVEN: coordinates of the mouse button-up event and the event
    ;; RETURNS: An unselected World
    ;;  A World% with no toys and an selected target at (200,250) will be
    ;;  unselected after a button-up at (210,250)
    ;; STRATEGY: function composition
    (define/public (world-after-button-up mx my mev)
      (send this world-after-mouse-event x y mx my false mev))
    
    ;; world-after-drag : Integer Integer -> Void
    ;; GIVEN: coordinates of the mouse drag event
    ;; RETURNS: A World with target moved to the given mouse pointer location
    ;; Example:
    ;;  A World% with no toys and a selected target at (200,250) will be
    ;;  smoothly dragged to (190,200) when the mouse pointer is dragged from
    ;;  (210,250) to (200,200)
    ;; STRATEGY: HOFC
    (define/public (world-after-drag mx my mev)
      (begin
        (target-after-drag mx my mev)
        (set! toys (map
                    (; StatefulToy<%> -> StatefulToy<%>
                     ;GIVEN: a toy in the world
                     ;RETURNS: the toy with its list of buddies updated with any
                     ;         new toys are currently overlapping the given toy
                     lambda (each-toy)
                      (toy-before-drag each-toy mx my mev))
                    toys))))
    
    ;; toy-before-drag : StatefulToy<%> Integer Integer MouseEvent 
    ;;                          -> StatefulToy<%>
    ;; GIVEN: a toy in the world, the location of a mouse pointer and the drag
    ;;        event
    ;; RETURNS: the toy with its list of buddies updated with any new toys that
    ;;          are currently overlapping the given toy
    ;; Example:
    ;; STRATEGY:
    (define (toy-before-drag toy mx my mev)
      (begin
        (for-each
         (; StatefulToy<%> -> Void
          ;GIVEN: a toy in the world
          ;EFFECT: if a toy is not already this toy's buddy then it is
          ;        added to the list of buddies of this toy
          lambda (other-toy)
           (when (and (not (equal? toy other-toy))
                      (send toy buddy? other-toy))
             (begin
               (send toy make-buddy other-toy)
               (send other-toy update-offsets mx my)
               (send other-toy make-buddy toy))))
         toys) toy))
    
    ;; target-after-drag : Integer Integer MouseEvent -> Void
    ;; GIVEN: the location of the mouse pointer on drag and the event 
    ;; EFFECT: moves the target smoothly to the new location of mouse
    ;;         pointer iff the target was selected
    ;; Example:
    ;;  A World% with no toys and a selected target at (200,250) will be dragged
    ;;  to (210,250) when mouse was dragged from (210,250) to (220,250)
    ;; STRATEGY: function composition
    (define (target-after-drag mx my mev)
      (local
        ((define delta-x (- mx x-click))
         (define delta-y (- my y-click)))
        (if selected?
            (send this world-after-mouse-event 
                  (+ x delta-x) (+ y delta-y) mx my selected? mev)
            (send this world-after-mouse-event
                  x y mx my selected? mev))))
    
    ;; world-after-mouse-event: Integer Integer Integer Integer Boolean 
    ;;                          MouseEvent -> Void
    ;; GIVEN: the location of the target's centre and mouse pointer, and the
    ;;        boolean value of selected?, and the mouse event that occured
    ;; EFFECT: the world as it should be after the given mouse event
    ;; Example:
    ;;  world-after-mouse-event will select the target with an unselected target
    ;;  at (200,250) for an input of 200,250,210,250,true,"button-down"
    ;; STRATEGY: HOFC
    (define/public (world-after-mouse-event new-x new-y mx my new-sel? mev)
      (set! x new-x)
      (set! y new-y)
      (set! x-click mx)
      (set! y-click my)
      (set! selected? new-sel?)
      (set! toys (map
                  (; StatefulToy<%> -> Void
                   ; GIVEN: each toy in the world
                   ; EFFECT: a toy with its state updated for the occured 
                   ;         MouseEvent
                   lambda (toy) (send toy on-mouse mx my mev))
                  toys)))
    
    
    ;; on-key : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the state of the world to a state that should be after
    ;;         the given key event
    ;; DETAILS: on 's', create a square toy; else ignore
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: Cases on kev : KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev S)
         (set! toys (cons (make-square-toy x y) toys))]
        [else this]))
    
    ;; on-draw : -> Scene
    ;; RETURNS: a scene like the given one, but with this world painted on it
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: HOFC
    (define/public (on-draw)
      (local 
        ((define TARGET-IMG 
           (if selected? SELECTED-TARGET-IMG UNSELECTED-TARGET-IMG)))
        (foldr
         (; StatefulToy<%> Scene -> Scene
          ;GIVEN: a stateful-toy and a scene painted so far
          ;RETURNS: a scene with the given toy painted on it
          lambda (toy rest)
           (send toy add-to-scene rest))
         (place-image TARGET-IMG x y EMPTY-CANVAS)
         toys)))
    
    ;; target-x: -> Integer
    ;; RETURNS: the x coordinate of the target
    (define/public (target-x)
      x)
    
    ;; target-y: -> Integer
    ;; RETURNS: the y coordinate of the target
    (define/public (target-y)
      y)
    
    ;; target-selected?: -> Boolean
    ;; RETURNS: true iff the target is selected?
    (define/public (target-selected?)
      selected?)
    
    ;; target-color : -> ColorString
    ;; RETURNS: color of the target
    (define/public (target-color)
      (if selected? SELECTED-TARGET-COLOR UNSELECTED-TARGET-COLOR))
    
    ;; get-toys: -> ListOfStatefulToy<%>
    ;; GIVEN: No arguments
    ;; RETURNS: the list of toys in the world
    (define/public (get-toys)
      toys) 
    
    ;; for-test:get-x-click: -> Integer
    ;; GIVEN: No arguments
    ;; RETURNS: the x-click of the world
    (define/public (for-test:get-x-click)
      x-click)
    
    ;; for-test:get-y-click: -> Integer
    ;; GIVEN: No arguments
    ;; RETURNS: the y-click of the world
    (define/public (for-test:get-y-click)
      y-click)
    
    ))

;===============================================================================

;;SquareToy% 
;; A SquareToy is a
;; (new SquareToy% [x PosInt]
;;                 [y PosInt]
;;                 [x-off NonNegInt]
;;                 [y-off NonNegInt]
;;                 [selected? Boolean]
;;                 [buddy-sel? Boolean]
;;                 [buddies ListOfStatefulToy<%>]
;; Interpretation: represents a SquareToy with its list of buddies in the world
(define SquareToy%               
  (class* object% (StatefulToy<%>)         
    (init-field x y)               ; x & y co-ordinates of center of square toy
    (init-field [x-off ZERO]       ; the manhattan-distance between mouse 
                [y-off ZERO])      ; pointer location and the toy's centre
    ;                                on the last mouse event
    (init-field [selected? false]) ; true iff the toy is selected
    (init-field [buddy-sel? false]); true iff any of this toy's buddies is 
    ;                                selected
    (init-field [buddies empty])   ; a list of toys that are buddies to this toy
    
    ;; Example:
    
    (super-new)
    
    ;; on-mouse : Integer Integer MouseEvent -> Void
    ;; EFFECT: updates this StatefulToy<%> to the state that it should be in
    ;;         after the given MouseEvent
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: Cases on mev : MouseEvent
    (define/public (on-mouse mx my mev)
      (cond
        [(mouse=? mev BUTTON-DOWN) 
         (send this toy-after-button-down mx my)]
        [(mouse=? mev DRAG)
         (send this toy-after-drag mx my)]
        [(mouse=? mev BUTTON-UP)
         (send this toy-after-button-up mx my)]
        [else this]))
    
    ;; toy-after-button-down : Integer Integer -> StatefulToy<%>
    ;; GIVEN: coordinates of the mouse button-down event
    ;; RETURNS: A selected toy iff the event occured within this toy
    ;; Example:
    ;;  A StatefulToy<%> at (200,250) will be selected after a button-down at
    ;; (210,250)
    ;; STRATEGY: HOFC
    (define/public (toy-after-button-down mx my)
      (begin
        (when (send this click-in-toy? mx my)
          (begin
            (set! x-off (- mx x))
            (set! y-off (- my y))
            (set! selected? true)
            (for-each
             (; StatefulToy<%> -> Void
              ;GIVEN: a toy which is a buddy to the current toy
              ;EFFECT: the buddy is updated as it should be after a
              ;        button-down in the current toy
              lambda (each-buddy)
               (send each-buddy buddy-after-button-down mx my))
             buddies)))
        this))
    
    ;; buddy-after-button-down : Integer Integer -> StatefulToy<%>
    ;; GIVEN: the location of the mouse pointer
    ;; RETURNS: the toy with its offsets updated and its buddy-sel? set to true
    ;; Example:
    ;;  Consider a toy is selected with mouse pointer at (210,250). Consider it
    ;;  has a buddy at (100,100), then its offsets are set to (110,150) and its
    ;;  buddy-sel? falg is set to true
    ;; STRATEGY: function composition
    (define/public (buddy-after-button-down mx my)
      (set! x-off (- mx x))
      (set! y-off (- my y))
      (set! buddy-sel? true)
      this)
    
    ;; update-offsets : Integer Integer -> Void
    ;; GIVEN: the location of the mouse pointer
    ;; EFFECT: the toy's offsets are updated
    ;; Example:
    ;;  Consider toy is at (100,100), then its offsets are set to (110,150) when
    ;;  mouse event occured at (210,250)
    ;; STRATEGY: function composition
    (define/public (update-offsets mx my)
      (set! x-off (- mx x))
      (set! y-off (- my y)))
    
    ;; click-in-toy? : Integer Integer -> Boolean
    ;; GIVEN: coordinates of the mouse button-down event
    ;; RETURNS: true iff the mouse button-down event occured within the toy
    ;; Example:
    ;;  click-in-toy? will return true for a StatefulToy<%> at (200,250)
    ;;  and mouse click at (210,250)
    ;; STRATEGY: function composition
    (define/public (click-in-toy? mx my)
      (and (<= (- x HALF-SQ-TOY-LEN) mx (+ x HALF-SQ-TOY-LEN))
           (<= (- y HALF-SQ-TOY-LEN) my (+ y HALF-SQ-TOY-LEN))))
    
    ;; toy-after-button-up : Integer Integer -> StatefulToy<%>
    ;; GIVEN: coordinates of the mouse button-up event
    ;; RETURNS: this toy with its selected? and buddy-sel? reset to false
    ;; Example:
    ;;  A StatefulToy<%> at (200,250) will be unselected after a button-up at
    ;;  (210,250)
    ;; STRATEGY: function composition
    (define/public (toy-after-button-up mx my)
      (set! selected? false)
      (set! buddy-sel? false)
      this)
    
    ;; toy-after-drag : Integer Integer -> StatefulToy<%>
    ;; GIVEN: coordinates of the mouse drag event
    ;; RETURNS: toy moved to the given mouse pointer location and its buddies
    ;;          moved bu the same offset
    ;; Example:
    ;;  A selected toy at (200,250) will be moved to (250,250) when mouse is
    ;;  moved from (210,250) to (260,250). Consider it had a buddy at (100,100).
    ;;  Now it'll be moved to (150,100)
    ;; STRATEGY: HOFC
    (define/public (toy-after-drag mx my)
      (begin
        (when selected?
          (begin
            (set! x (- mx x-off))
            (set! y (- my y-off))
            (set! x-off (- mx x))
            (set! y-off (- my y))
            (for-each
             (; StatefulToy<%> -> Void
              ;GIVEN: a toy that is a buddy to the current toy
              ;EFFECT: the buddy toy is also dragged by the same distance as
              ;        the current toy
              lambda (each-buddy)
               (send each-buddy buddy-after-drag mx my))
             buddies)))
        this))
    
    ;; buddy-after-drag : Integer Integer -> Void
    ;; GIVEN: the location of the mouse pointer
    ;; EFFECT: moves the buddy toy by the same offset as its buddy that was
    ;;         selected and dragged
    ;; Example:
    ;;  A buddy toy at (100,100) will be moved to (150,100) when its buddy that
    ;;  was selected was moved by the same offset
    ;; STRATEGY: function composition
    (define/public (buddy-after-drag mx my)
      (set! x (- mx x-off))
      (set! y (- my y-off))
      (set! buddy-sel? true))
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the given one, but with this toy drawn on it
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: function composition
    (define/public (add-to-scene scene)
      (local
        ((define SQ-TOY (if (or selected? buddy-sel?)
                            SELECTED-SQ-TOY-IMG
                            UNSELECTED-SQ-TOY-IMG)))
        (place-image SQ-TOY x y scene)))
        
    ;; buddy? : StatefulToy<%> -> Boolean
    ;; GIVEN: a toy
    ;; RETURNS: true iff this toy is intersecting with the given toy
    ;; Example:
    ;;  Consider this toy was at (200,250). If a toy at (210,250) was given
    ;;  to this function, it would return true
    ;; STRATEGY: function composition
    (define/public (buddy? other-toy)
      (local
        ((define other-x-left (- (send other-toy toy-x) HALF-SQ-TOY-LEN))
         (define other-x-right (+ (send other-toy toy-x) HALF-SQ-TOY-LEN))
         (define other-y-top (- (send other-toy toy-y) HALF-SQ-TOY-LEN))
         (define other-y-bottom (+ (send other-toy toy-y) HALF-SQ-TOY-LEN))
         (define x-left (- x HALF-SQ-TOY-LEN))
         (define x-right (+ x HALF-SQ-TOY-LEN))
         (define y-top (- y HALF-SQ-TOY-LEN))
         (define y-bottom (+ y HALF-SQ-TOY-LEN)))
        (and (<= x-left other-x-right)
             (>= x-right other-x-left)
             (<= y-top other-y-bottom)
             (>= y-bottom other-y-top))))
    
    ;; make-buddy : StatefulToy<%> -> Void
    ;; GIVEN: a toy
    ;; EFFECT: adds the given toy to this toy's buddies if it doesn't already
    ;;         exist
    ;; Example:
    ;;  If a toy at (210,250) was given input to this function for a toy at
    ;;  (200,250) and it wasn't already in the buddies, then the buddies will
    ;;  be updated by adding the given toy to it
    ;; STRATEGY: function composition
    (define/public (make-buddy some-toy)
      (if (buddy-exists? some-toy)
          (set! buddies buddies) 
          (set! buddies (cons some-toy buddies))))
    
    ;; buddy-exists? : StatefulToy<%> -> Boolean
    ;; GIVEN: a toy
    ;; RETURNS: true iff it already is in the buddies of the current toy
    ;; Example:
    ;;  if a toy in input to this function and it is already present in the
    ;;  buddies, then it returns true
    ;; STRATEGY: HOFC
    (define/public (buddy-exists? t)
      (ormap
       (; StatefulToy<%> -> Boolean
        ;GIVEN: a buddy toy from the buddies of the current toy
        ;RETURNS: true iff the given toy and the buddy toy are same
        lambda(each-buddy)
         (equal? each-buddy t))
       buddies))
    
    
    ;; toy-x : -> PosInt
    ;; RETURNS: the x coordinate of the centre of the toy
    (define/public (toy-x)
      x)
    
    ;; toy-y : -> PosInt
    ;; RETURNS: the y coordinate of the centre of the toy
    (define/public (toy-y)
      y)
    
    ;; toy-selected? : -> Boolean
    ;; RETURNS: true iff the toy is selected
    (define/public (toy-selected?)
      selected?)
    
    ;; toy-color : -> ColorString
    ;; RETURNS: the color of the toy
    (define/public (toy-color)
      (if (or selected? buddy-sel?)
          SELECTED-SQ-TOY-COLOR
          UNSELECTED-SQ-TOY-COLOR))
    
    ;; toy-buddy-sel? : -> Boolean
    ;; RETURNS: true iff any of this toy's buddies is selected
    (define/public (toy-buddy-sel?)
      buddy-sel?)
    
    ;; get-buddies : ListOfStatefulToy<%>
    ;; RETURNS: the list toys that are buddy to this toy
    (define/public (get-buddies)
      buddies)
    
    ))

;===============================================================================

;;make-square-toy: PosInt PosInt -> SquareToy
;;GIVEN: the coordinates of the centre of a square toy to be created
;;RETURNS: a SquareToy with the given values
;;EXAMPLE: See test cases
;;STRATEGY: function composition
(define (make-square-toy x y)
  (new SquareToy% 
       [x x] 
       [y y]))

;===============================================================================
;===============================================================================

;run : PosNum -> World<%>
;GIVEN: a frame rate (in seconds/tick)
;EFFECT: creates and runs a world.  Returns the final state of the world.
(define (run frame-rate)
  (big-bang (make-world)
            (on-tick
             (; World<%> -> World<%>
              ;GIVEN: a world
              ;RETURNS: a world just like the given, but as it should be after a
              ;         tick
              lambda (w) (send w on-tick) w)
             frame-rate)
            (on-draw
             (; World<%> -> World<%>
              ;GIVEN: a world
              ;RETURNS: the given world painted on a canvas
              lambda (w) (send w on-draw)))
            (on-key
             (; World<%> KeyEvent -> World<%>
              ;GIVEN: a world and a key event
              ;RETURNS: a world just like the give, but as it should be after
              ;         the effect of the given key event on it
              lambda (w kev) (send w on-key kev) w))
            (on-mouse
             (; World<%> Integer Integer MouseEvent -> World<%>
              ;GIVEN: a world, location of the mouse pointer and a mouse event
              ;RETURNS: a world just like the given, but a it should be after
              ;         the effect of the given mouse event on it
              lambda (w x y evt) (send w on-mouse x y evt) w))))

;===============================================================================

;; make-world : -> World<%>
;; RETURNS: a world with an unselected target and no toys
;; Examlpes:
;;  (make-world 10) => (new World% [x 200] [y 250] [x-click 0] [y-click 0]
;;                         [selected? false] [toys empty])
;; STRATEGY: function composition
(define (make-world)
  (new World% 
       [x CANVAS-X-CENTER]
       [y CANVAS-Y-CENTER]
       [x-click ZERO]
       [y-click ZERO]
       [selected? false]
       [toys empty]))   

;===============================================================================

;;TESTS:

;; Functions for Tests:
;;world-equal? : StatefulWorld<%> StatefulWorld<%> -> Boolean
;;GIVEN: two worlds
;;RETURNS: true iff the two worlds are exactly identical
;;STRATEGY: HOFC
(define (world-equal? w1 w2)
  (and
   (= (send w1 target-x) (send w2 target-x))
   (= (send w1 target-y) (send w2 target-y))
   (equal? (send w1 target-selected?) (send w2 target-selected?))
   (= (send w1 for-test:get-x-click) (send w2 for-test:get-x-click))
   (= (send w1 for-test:get-y-click) (send w2 for-test:get-y-click))))

;;TEST CONSTANTS:
(define init (new World% [x CANVAS-X-CENTER] [y CANVAS-Y-CENTER] [x-click ZERO]
                  [y-click ZERO] [selected? false] [toys empty]))

(begin-for-test
  (check-equal? (world-equal? (make-world) init) true
                "world initiation should be proper")
  (local
    ((define world (new World%
                            [x CANVAS-X-CENTER]
                            [y CANVAS-Y-CENTER]
                            [x-click ZERO]
                            [y-click ZERO] 
                            [selected? false]
                            [toys empty])))
    (send world on-tick)
    (check-equal? (send world get-toys) empty)
    (send world on-key NON-RESPONSIVE-KEY-EVENT)
    
    (send world on-key S)
    (define first-toy (first (send world get-toys)))
    (check-equal? (send first-toy toy-x)
                  (send world target-x)
                  "the new toy should be created where the target is present")
    (check-equal? (send first-toy toy-y)
                  (send world target-y)
                  "the new toy should be created where the target is present")
    ;MOUSE-BUTTON-DOWN
    (send world on-mouse 210 250 NON-RESPONSIVE-MOUSE-EVENT)
    (send first-toy on-mouse 210 250 NON-RESPONSIVE-MOUSE-EVENT)
    (send world on-mouse 210 250 BUTTON-DOWN)
    (send world on-draw)
    (check-equal? (send world target-selected?)
                  true
                  "on a button-down inside the target, the target should be
                   made selected")
    (check-equal? (send world target-color)
                  SELECTED-TARGET-COLOR
                  "a selected target should be orange in color")
    (check-equal? (send first-toy toy-selected?)
                  true
                  "on a button-down inside a toy, the target should be made
                   selected")
    ;MOUSE-BUTTON-UP
    (send world on-mouse 210 250 BUTTON-UP)
    (check-equal? (send world target-selected?)
                  false
                  "on a button-up, the target should be unselected")
    (check-equal? (send world target-color)
                  UNSELECTED-TARGET-COLOR
                  "an unselected target should be black in color")
    (check-equal? (send first-toy toy-selected?)
                  false
                  "on a button-up, a toy should be unselected")
    (check-equal? (send first-toy toy-color)
                  UNSELECTED-SQ-TOY-COLOR
                  "an unselected toy should be green in color")
    ;MOUSE-BUTTON-DOWN in toy
    (send world on-mouse 212 250 BUTTON-DOWN)
    (check-equal? (send world target-selected?)
                  false
                  "a button-down outside the target should not select it")
    (check-equal? (send first-toy toy-selected?)
                  true
                  "on a button-down inside a toy, the target should be made
                   selected")
    (check-equal? (send first-toy toy-color)
                  SELECTED-SQ-TOY-COLOR
                  "a selected toy should be red in color")
    ;MOUSE-DRAG in toy, to (200,150)
    (send world on-mouse 212 150 DRAG)
    (check-equal? (send first-toy toy-x)
                  200
                  "a toy should be smoothly dragged when selected")
    (check-equal? (send first-toy toy-y)
                  150
                  "a toy should be smoothly dragged when selected")
    (send world on-mouse 212 150 BUTTON-UP)
    
    ;MOUSE-DRAG in target
    (send world on-mouse 200 250 BUTTON-DOWN)
    (send world on-mouse 150 200 DRAG)
    (send world on-draw)
    (check-equal? (send world target-x)
                  150
                  "the target should be smoothly dragged when selected")
    (check-equal? (send world target-y)
                  200
                  "the target should be smoothly dragged when selected")
    ;SQUARE-TOY2
    (send world on-key S)
    (define second-toy (first (send world get-toys)))
    (check-equal? (length (send world get-toys))
                  2
                  "a new toy should be created on an 's' key event")
    (send world on-mouse 150 200 BUTTON-UP)
    (send world on-mouse 165 215 BUTTON-DOWN)
    (check-equal? (send second-toy toy-selected?)
                  true
                  "a button-down inside a toy should select it")
    ;DRAG second-toy to (300,300)
    (send world on-mouse 315 315 DRAG)
    (send world on-mouse 315 315 BUTTON-UP)
    
    ;Select first-toy
    (send world on-mouse 200 150 BUTTON-DOWN)
    (send world on-mouse 285 284 DRAG)
    (check-equal? (send first-toy toy-x)
                  285
                  "a toy should be smoothly dragged when selected")
    (check-equal? (send first-toy toy-y)
                  284
                  "a toy should be smoothly dragged when selected")
    (check-equal? (send first-toy toy-selected?)
                  true
                  "a toy should remain selected till button-up")
    (check-equal? (send second-toy toy-x)
                  300
                  "an unselected-toy should not move on drag")
    (check-equal? (send second-toy toy-y)
                  300
                  "an unselected-toy should not move on drag")
    (check-equal? (send second-toy toy-selected?)
                  false
                  "an unselected toy should remain that way during a drag")
    (send world on-mouse 285 285 DRAG)
    (check-equal? (send first-toy toy-x)
                  285
                  "on drag, the toy sould be smoothly dragged")
    (check-equal? (send first-toy toy-y)
                  285
                  "on drag, the toy sould be smoothly dragged")
    (check-equal? (send first-toy toy-selected?)
                  true
                  "while being dragged, toy remains selected")
    (check-equal? (send second-toy toy-x)
                  300
                  "when two toys overlap, they become buddies")
    (check-equal? (send second-toy toy-y)
                  301
                  "when two toys overlap, they become buddies")
    (check-equal? (send second-toy toy-selected?)
                  false
                  "a buddy will not be selected when its buddy is being draged")
    (check-equal? (send second-toy toy-buddy-sel?)
                  true
                  "a buddy's 'buddy-sel?' is set to true when its buddy is 
                   being dragged")
    (check-equal? (length (send first-toy get-buddies))
                  1
                  "'buddies' is a list of buddy toys to this toy")
    (check-equal? (length (send second-toy get-buddies))
                  1
                  "'buddies' is a list of buddy toys to this toy")
    (send world on-mouse 285 285 BUTTON-UP)
    ;buddy after button-down    
    (send world on-mouse 300 301 BUTTON-DOWN)
    (check-equal? (send second-toy toy-selected?)
                  true
                  "a toy will be selected on a button-down inside it")
    (check-equal? (send first-toy toy-buddy-sel?)
                  true
                  "a toy's 'buddy-sel?' is true when is buddy is selected")
    (send world on-draw)))