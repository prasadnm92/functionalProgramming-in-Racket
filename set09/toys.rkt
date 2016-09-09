#lang racket
;;PURPOSE STATEMENT:
;; this file contains the solution to the implementation of 'Marvelous Toys'
;; (not the song :D), which contains a target and a list of toys. A toy can be a
;; square toy or circle toy. The target is used to place toys on the canvas.

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image) 

(provide World%
         SquareToy%
         CircleToy%
         make-world
         run
         make-square-toy
         make-circle-toy
         World<%>
         Toy<%>)

;; CONSTANTS:

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define ZERO 0)
(define ONE 1)
(define FIVE 5)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-X-CENTER 200)
(define CANVAS-Y-CENTER 250)

(define SOLID "solid")
(define OUTLINE "outline")

(define TARGET-RADIUS 10)
(define TARGET-COLOR "blue")
(define TARGET-IMG (circle TARGET-RADIUS OUTLINE TARGET-COLOR))
(define SQ-TOY-LEN 40)
(define SQ-TOY-COLOR "purple")
(define SQ-TOY (square SQ-TOY-LEN OUTLINE SQ-TOY-COLOR))

(define C-TOY-RADIUS 5)
(define COLOR1 "green")
(define COLOR2 "red")

(define X-MAX 380)
(define X-MIN 20)
(define RIGHT "right")
(define LEFT "left")

;===============================================================================
;===============================================================================

;; DATA DEFINITIONS:

;A Direction is one of:
; -- "right"                            interp: SquareToy is moving right
; -- "left"                             interp: SquareToy is moving left
; TEMPLATE:
; direction-fn : Direction -> ??
#;(define (direction-fn dir)
    (cond
      [(string=? dir "right")...]
      [(string=? dir "left")...]))

;A ColorString is one of:
; -- "purple"                           interp: indicates a purple color
; -- CircleToyColor                     interp: describes the color of a circle 
;                                               toy
; TEMPLATE:
; col-fn : ColorString -> ??
#;(define (col-fn col)
    (cond
      [(string=? col "purple") ...]
      [else (... (c-toy-col-fn col))]))

;A CircleToyColor is one of:
; -- "green"                            interp: the CircleToy is green in color
; -- "red"                              interp: the CircleToy is red in color
; TEMPLATE:
; c-toy-col-fn : CircleToyColor -> ??
#;(define (c-toy-col-fn col)
    (cond
      [(string=? col "green") ...]
      [(string=? col "red") ...]))

;A ListOf<Toy<%>> is one of:
; -- empty                              interp: an empty list of toys
; -- (cons Toy<%> ListOf<Toy<%>>)       interp: a non-empty list of toys
; TEMPLATE:
; lst-fn : ListOf<Toy<%>> -> ??
#;(define (lst-fn lst)
    (cond
      [(empty? lst) ...]
      [else (... (... (first lst))
                 (lst-fn (rest lst)))]))

;A ListOfFields is one of:
; -- (list PosInt PosInt PosInt Direction ColorString)     interp: a list of 
;                                                            square toy fields
; -- (list PosInt PosInt CircleToyColor PosInt)            interp: a list of 
;                                                            circle toy fields
; TEMPLATE:
; lst-of-fields-fn : ListOfFields -> ??
#;(define (lst-of-fields-fn lst)
    (cond
      [(= (length lst) 5) ...]
      [(= (length lst) 4) ...]))


;===============================================================================
;===============================================================================

;;Interfaces:

(define World<%>
  (interface ()
    
    ;; -> World<%>
    ;; Given: No arguments
    ;; Returns: the World<%> that should follow this one after a tick
    on-tick                             
    
    ;; Integer Integer MouseEvent -> World<%>
    ;; Given: the location of the mouse pointer and a mouse event
    ;; Returns: the World<%> that should follow this one after the
    ;;          given MouseEvent
    on-mouse
    
    ;; KeyEvent -> World<%>
    ;; Given: a key event that has occured
    ;; Returns: the World<%> that should follow this one after the
    ;;          given KeyEvent
    on-key
    
    ;; -> Scene
    ;; Given: No arguments
    ;; Returns: a Scene depicting this world on it
    on-draw
    
    ;; -> Integer
    ;; Given: No arguments
    ;; RETURNS: the x and y coordinates of the centre target
    target-x
    target-y
    
    ;; -> Boolean
    ;; Given:
    ;; Returns: true iff the target is selected; false otherwise
    target-selected?
    
    ;; -> ListOfToy<%>
    ;; GIVEN: No arguments
    ;; RETURNS: the list of toys in the world 
    get-toys
    
    ;; -> Integer
    ;; GIVEN: No arguments
    ;; RETURNS: the x-click of the world
    for-test:get-x-click
    
    ;; -> Integer
    ;; GIVEN: No arguments
    ;; RETURNS: the y-click of the world
    for-test:get-y-click
    
    ;; -> PosInt
    ;; GIVEN: No arguments
    ;; RETURNS: the square-speed of the world
    for-test:get-speed
    
    ))

;===============================================================================

(define Toy<%> 
  (interface ()
    
    ;; -> Toy<%>
    ;; Given: No arguments
    ;; Returns: the Toy that should follow this one after a tick
    on-tick                             
    
    ;; Scene -> Scene
    ;; Given: a scene
    ;; Returns: a Scene like the given one, but with this toy drawn on it
    add-to-scene
    
    ;; -> PosInt
    ;; Given: No arguments
    ;; Returns: the x and y coordinates of the centre of the toy
    toy-x
    toy-y
    
    ;; -> ColorString
    ;; Given: No arguments
    ;; Returns: the current color of this toy
    toy-color
    
    ;; -> ListOfFields
    ;; Given: No arguments
    ;; Returns: a list of the fields of this toy
    for-test:get-fields
    
    ))


;===============================================================================
;===============================================================================

;; Classes:

;;World%  
;; A World is a
;; (new World% [x PosInt]            
;;             [y PosInt]             
;;             [x-click Integer]     
;;             [y-click Integer]      
;;             [selected? Boolean]    
;;             [square-speed PosInt]  
;;             [toys ListOf<Toy>])    
;; Interpretation: represents a world, containing a target and some toys.
(define World%               
  (class* object% (World<%>)         
    (init-field x)           ;the x coordinate of the target
    (init-field y)           ;the y coordinate of the target
    (init-field x-click)     ;the x coordinate of the mouse pointer on the last
    ;                             mouse event
    (init-field y-click)     ;the y coordinate of the mouse pointer on the last
    ;                             mouse event
    (init-field selected?)   ;true iff the target is selected
    (init-field square-speed);the speed of the square toy in pixels/tick
    (init-field toys)        ; a ListOf<Toy<%>> -- the list of toys in the game 
    
    ;;Example: 
    #;(new World% 
           [x 200]
           [y 250]
           [x-click 210]
           [y-click 250]
           [selected? true]
           [square-speed 10]
           [toys empty])
    
    (super-new)
    
    ;; on-tick : -> World<%>
    ;; GIVEN: No arguments
    ;; RETURNS: A world like this one, but as it should be after a tick
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: HOFC
    (define/public (on-tick)
      (new World%
           [x x]
           [y y]
           [x-click x-click]
           [y-click y-click]
           [selected? selected?]
           [square-speed square-speed]
           [toys (map
                  (; Toy<%> -> Toy<%>
                   ;GIVEN: a toy
                   ;RETURNS: a toy like the given, but as it should be after 
                   ;         a tick
                   lambda (toy) (send toy on-tick))
                  toys)]))
    
    ;; on-mouse : Integer Integer MouseEvent -> World<%>
    ;; GIVEN: the location of the mouse pointer and a mouse event
    ;; RETURNS: A world like this one, but as it should be after the
    ;;          given mouse event
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: Cases on evt : MouseEvent
    (define/public (on-mouse x y evt)
      (cond
        [(mouse=? evt "button-down") 
         (send this world-after-button-down x y)]
        [(mouse=? evt "drag")
         (send this world-after-drag x y)]
        [(mouse=? evt "button-up")
         (send this world-after-button-up x y)]
        [else this]))
    
    
    ;; world-after-button-down : Integer Integer -> World<%>
    ;; GIVEN: coordinates of the mouse button-down event
    ;; RETURNS: A selected World if the event occured within the target, else
    ;;          the same World
    ;; Example:
    ;;  A World% with an unselected target at (200,250) will be selected after
    ;;  a button-down at (210,250)
    ;; STRATEGY: function composition
    (define/public (world-after-button-down mx my)
      (if (send this click-in-target? mx my)
          (send this world-after-mouse-event x y mx my true)
          (send this world-after-mouse-event x y mx my selected?)))
    
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
    
    ;; world-after-button-up : Integer Integer -> World<%>
    ;; GIVEN: coordinates of the mouse button-up event
    ;; RETURNS: An unselected World
    ;;  A World% with an selected target at (200,250) will be unselected after
    ;;  a button-up at (210,250)
    ;; STRATEGY: function composition
    (define/public (world-after-button-up mx my)
      (send this world-after-mouse-event x y mx my false))
    
    ;; world-after-drag : Integer Integer -> World<%>
    ;; GIVEN: coordinates of the mouse drag event
    ;; RETURNS: A World with target moved to the given mouse pointer location
    ;; Example:
    ;;  A World% with a selected target at (200,250) will be smoothly dragged to
    ;;  (190,200) when the mouse pointer is dragged from (210,250) to (200,200)
    ;; STRATEGY: function composition
    (define/public (world-after-drag mx my)
      (local
        ((define delta-x (- mx x-click))
         (define delta-y (- my y-click)))
        (if selected?
            (send this world-after-mouse-event 
                  (+ x delta-x) (+ y delta-y) mx my selected?)
            (send this world-after-mouse-event
                  x y mx my selected?))))
    
    ;; world-after-mouse-event: Integer Integer Integer Integer Boolean 
    ;;                          -> World<%>
    ;; GIVEN: the location of the target's centre and mouse pointer, and the
    ;;        boolean value of selected?
    ;; RETURNS: a World with the given values
    ;; Example:
    ;;  world-after-mouse-event will make a new World% with an unselected target
    ;;  at (200,250) for an input of 200,250,PosInt,PosInt,false
    ;; STRATEGY: function composition
    (define/public (world-after-mouse-event x y mx my selected?)
      (new World%
           [x x]
           [y y]
           [x-click mx]
           [y-click my]
           [selected? selected?]
           [square-speed square-speed]
           [toys toys]))
    
    
    ;; on-key : KeyEvent -> World<%>
    ;; GIVEN: a key event
    ;; RETURNS: A world like this one, but as it should be after given key event
    ;; DETAILS: on 's', create a square toy; on 'c', create a circle toy
    ;;          else ignore
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: Cases on kev : KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev "s") (new World%
                              [x x]
                              [y y]
                              [x-click x-click]
                              [y-click y-click]
                              [selected? selected?] 
                              [square-speed square-speed]
                              [toys 
                               (cons (make-square-toy x y square-speed) toys)])]
        [(key=? kev "c") (new World%
                              [x x]
                              [y y]
                              [x-click x-click]
                              [y-click y-click]
                              [selected? selected?]
                              [square-speed square-speed]
                              [toys (cons (make-circle-toy x y) toys)])]
        [else this]))    
    
    ;; on-draw : -> Scene
    ;; GIVEN: No arguments
    ;; RETURNS: a scene like the given one, but with this world painted on it
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: HOFC
    (define/public (on-draw)
      (foldr
       (; Toy<%> Scene -> Scene
        ;GIVEN: a toy and a scene painted so far
        ;RETURNS: a scene with the given toy painted on it
        lambda (toy rest)
         (send toy add-to-scene rest))
       (place-image TARGET-IMG x y EMPTY-CANVAS)
       toys)) 
    
    ;; target-x: -> Integer
    ;; GIVEN: No arguments
    ;; RETURNS: the x coordinate of the target
    (define/public (target-x)
      x)
    
    ;; target-y: -> Integer
    ;; GIVEN: No arguments
    ;; RETURNS: the y coordinate of the target
    (define/public (target-y)
      y)
    
    ;; target-selected?: -> Boolean
    ;; GIVEN: No arguments
    ;; RETURNS: true iff the target is selected?
    (define/public (target-selected?)
      selected?)
    
    ;; get-toys: -> ListOfToy<%>
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
    
    ;; for-test:get-speed: -> PosInt
    ;; GIVEN: No arguments
    ;; RETURNS: the square-speed of the world
    (define/public (for-test:get-speed)
      square-speed)
    
    ;; for-test:get-fields: -> (list Int Int Int Int boolean Int ListOf<Toy<%>>)
    ;; GIVEN: No arguments
    ;; RETURNS: a list of all the fields of this object
    (define/public (for-test:get-fields)
      (list (send this target-x)
            (send this target-y)
            (send this for-test:get-x-click)
            (send this for-test:get-y-click)
            (send this target-selected?)
            (send this for-test:get-speed)
            (send this get-toys)))
    
    ))

;===============================================================================

;;SquareToy% 
;; A SquareToy is a
;; (new SquareToy% [x PosInt]
;;                 [y PosInt]
;;                 [square-speed PosInt]
;;                 [dir Direction] 
;; Interpretation: represents a SquareToy moving with a given speed in a 
;;                 direction
(define SquareToy%               
  (class* object% (Toy<%>)         
    (init-field x)              ; x co-ordinate of center of square toy
    (init-field y)              ; y co-ordinate of center of square toy 
    (init-field square-speed)   ; speed of the square toy moves in pixels/tick 
    (init-field dir)            ; Direction in which the square toy is moving
    
    ;; Example:
    #;(new SquareToy%
           [x 200]
           [y 250]
           [square-speed 10]
           [dir "right"])
    
    
    (super-new)
    
    ;; on-tick : -> SquareToy
    ;; GIVEN: No arguments
    ;; RETURNS: A SquareToy like this one, but as it should be after a tick
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: SD dir : Direction
    (define/public (on-tick)
      (cond
        [(string=? dir RIGHT) (send this right-sq-toy-on-tick)]
        [(string=? dir LEFT) (send this left-sq-toy-on-tick)]))
    
    ;; right-sq-toy-on-tick : -> SquareToy
    ;; GIVEN: No arguments
    ;; RETURNS: a square toy just like the given, moved by 'square-speed' pixels
    ;;          to the right
    ;; Example:
    ;;  A SquareToy% at (200,250) moving towards 'right' at '10' speed will be
    ;;  at (210,250), moving towards 'right', on this tick
    ;; STRATEGY: function composition
    (define/public (right-sq-toy-on-tick)
      (local
        ((define new-dir (send this new-r-dir (+ x square-speed))) 
         (define new-x (min X-MAX (max X-MIN (+ x square-speed)))))
        (new SquareToy% [x new-x] [y y] [square-speed square-speed] 
             [dir new-dir])))
    
    ;; left-sq-toy-on-tick : -> SquareToy
    ;; GIVEN: No arguments
    ;; RETURNS: a square toy just like the given, moved by 'square-speed' pixels
    ;;          to the left
    ;; Example:
    ;;  A SquareToy% at (200,250) moving towards 'left' at '10' speed will be
    ;;  at (190,250), moving towards 'left', on this tick
    ;; STRATEGY: function composition
    (define/public (left-sq-toy-on-tick)
      (local
        ((define new-dir (send this new-l-dir (- x square-speed)))
         (define new-x (min X-MAX (max X-MIN (- x square-speed)))))
        (new SquareToy% [x new-x] [y y] [square-speed square-speed] 
             [dir new-dir])))
    
    ;; new-r-dir : PosInt -> Direction
    ;; GIVEN: the new x position of the toy, after applying speed
    ;; RETURNS: the direction of the toy, that should be, after applying the
    ;;          speed while moving right
    ;; Example:
    ;;  A SquareToy% at x coordinate 380 moving towards 'right' will turn toward
    ;;  'left'
    ;; STRATEGY: function composition
    (define/public (new-r-dir x-plus-speed)
      (if (>= x-plus-speed X-MAX)
          LEFT
          RIGHT))
    
    ;; new-l-dir : PosInt -> Direction
    ;; GIVEN: the new x position of the toy, after applying speed
    ;; RETURNS: the direction of the toy, that should be, after applying the
    ;;          speed while moving left
    ;; Example:
    ;;  A SquareToy% at x coordinate 20 moving towards 'left' will turn toward
    ;;  'right'
    ;; STRATEGY: function composition
    (define/public (new-l-dir x-minus-speed)
      (if (<= x-minus-speed X-MIN)
          RIGHT
          LEFT))
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the given one, but with this toy drawn on it
    ;; Example:
    ;;  See test cases
    ;; STRATEGY: function composition
    (define/public (add-to-scene scene)
      (place-image SQ-TOY x y scene))
    
    ;; toy-x : -> PosInt
    ;; GIVEN: No argument
    ;; RETURNS: the x coordinate of the centre of the toy
    (define/public (toy-x)
      x)
    
    ;; toy-y : -> PosInt
    ;; GIVEN: No argument
    ;; RETURNS: the y coordinate of the centre of the toy
    (define/public (toy-y)
      y)
    
    ;; toy-color : -> ColorString
    ;; GIVEN: No argument
    ;; RETURNS: the color of the toy
    (define/public (toy-color)
      SQ-TOY-COLOR)
    
    ;; for-test:toy-dir : -> Direction
    ;; GIVEN: No argument
    ;; RETURNS: the direction of the toy
    (define/public (for-test:toy-dir)
      dir)
    
    ;; for-test:toy-speed : -> PosInt
    ;; GIVEN: No argument
    ;; RETURNS: the speed of the toy
    (define/public (for-test:toy-speed)
      square-speed)
    
    ;; for-test:get-fields : -> (list Int Int PosInt Direction ColorString)
    ;; GIVEN: No argument
    ;; RETURNS: a list of all the fields of this toy
    (define/public (for-test:get-fields)
      (list (send this toy-x)
            (send this toy-y)
            (send this for-test:toy-speed) 
            (send this for-test:toy-dir) 
            (send this toy-color)))
    
    ))

;===============================================================================

;;make-square-toy: PosInt PosInt PosInt -> SquareToy
;;GIVEN: the coordinates of the centre of a square toy and its speed
;;RETURNS: a SquareToy with the given values
;;EXAMPLE: See test cases
;;STRATEGY: function composition
(define (make-square-toy x y square-speed)
  (new SquareToy% 
       [x x] 
       [y y]
       [square-speed square-speed]
       [dir RIGHT]))

;===============================================================================
;===============================================================================

;;CircleToy% 
;; A CircleToy is a
;; (new CircleToy% [x PosInt]
;;                 [y PosInt]
;;                 [tick-counter PosNum]
;;                 [color String] 
;; Interpretation: represents a CircleToy
(define CircleToy%               
  (class* object% (Toy<%>)         
    (init-field x)            ; x-coordinate of center of circle toy
    (init-field y)            ; y-coordinate of center of circle toy
    (init-field tick-counter) ; the current tick count for this toy 
    ;                           WHERE: the count starts from 1 and increments
    ;                                  by 1 on each tick up to 5. On reaching 5,
    ;                                  the tick counter resets to 1
    (init-field color)        ; the ColorString representing color of the toy
    
    ;;Example:
    #;(new CircleToy%
           [x 200]
           [y 250]
           [tick-counter 1]
           [color "green"])
    
    
    (super-new)
    
    ;; on-tick : -> CircleToy
    ;; GIVEN: No arguments
    ;; RETURNS: A CircleToy like this one, but with its color toggled if it has 
    ;;          been 5 ticks since the last toggle
    ;; Example:
    ;;  See tese cases
    ;; STRATEGY: function composition
    (define/public (on-tick)
      (local 
        ((define new-color (send this change-color color))
         (define new-tick-ctr (+ ONE tick-counter)))
        (if (= tick-counter FIVE)
            (new CircleToy% 
                 [x x] [y y] [tick-counter ONE] [color new-color])
            (new CircleToy% 
                 [x x] [y y] [tick-counter new-tick-ctr] [color color]))))
    
    ;; change-color : CircleToyColor -> CircleToyColor
    ;; GIVEN: the current color of this toy
    ;; RETURNS: the toggled color of this toy
    ;; Examples:
    ;;  (change-color COLOR1) => COLOR2
    ;;  (change-color COLOR2) => COLOR1
    ;; STRATEGY: SD on old-color : CircleToyColor
    (define/public (change-color old-color)
      (cond
        [(string=? old-color COLOR1) COLOR2]
        [(string=? old-color COLOR2) COLOR1]))
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the given one, but with this toy drawn on it
    ;; Example:
    ;; Consider a CircleToy% at (200,250)
    ;;  (add-to-scene 
    ;;    (place-image (circle 10 "outline" "blue") 200 250 EMPTY-CANVAS))
    ;;  => (place-image (square 40 "outline" SQ-TOY-COLOR) 200 250 
    ;;       (place-image (circle 10 "outline" "blue") 200 250 EMPTY-CANVAS))
    ;; STRATEGY: function composition
    (define/public (add-to-scene scene)
      (place-image (circle C-TOY-RADIUS SOLID color) x y scene))
    
    ;; toy-x : -> PosInt
    ;; GIVEN: No argument
    ;; RETURNS: the x coordinate of this toy's centre
    (define/public (toy-x)
      x)
    
    ;; toy-y : -> PosInt
    ;; GIVEN: No argument
    ;; RETURNS: the y coordinate of this toy's centre
    (define/public (toy-y)
      y)
    
    ;; toy-color : -> CircleToyColor
    ;; GIVEN: No argument
    ;; RETURNS: the color of this toy
    (define/public (toy-color)
      color)
    
    ;; for-test:tick-ctr : -> PosInt
    ;; GIVEN: No argument
    ;; RETURNS: the tick-counter of this toy
    (define/public (for-test:tick-ctr)
      tick-counter)
    
    ;; for-test:get-fields : -> (list PosInt PosInt CircleToyColor PosInt)
    ;; GIVEN: No argument
    ;; RETURNS: a list f all the fields of this toy
    (define/public (for-test:get-fields)
      (list (send this toy-x)
            (send this toy-y)
            (send this toy-color) 
            (send this for-test:tick-ctr)))
    
    ))

;===============================================================================

;;make-circle-toy: PosInt PosInt -> CircleToy
;;GIVEN: the coordinates of the centre of a circle toy
;;RETURNS: a CircleToy with the given values and its tick-counter initialised to
;;         one, its color initialised to "green"
;;Example:
;;  See test cases
;;STRATEGY: function composition
(define (make-circle-toy x y)
  (new CircleToy%
       [x x]
       [y y]
       [tick-counter ONE]
       [color COLOR1]))

;===============================================================================
;===============================================================================

;run : PosNum PosInt -> World<%>
;GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;creates and runs a world.  Returns the final state of the world.
(define (run frame-rate square-speed)
  (big-bang (make-world square-speed)
            (on-tick
             (; World<%> -> World<%>
              ;GIVEN: a world
              ;RETURNS: a world just like the given, but as it should be after a
              ;         tick
              lambda (w) (send w on-tick))
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
              lambda (w kev) (send w on-key kev)))
            (on-mouse
             (; World<%> Integer Integer MouseEvent -> World<%>
              ;GIVEN: a world, location of the mouse pointer and a mouse event
              ;RETURNS: a world just like the given, but a it should be after
              ;         the effect of the given mouse event on it
              lambda (w x y evt) (send w on-mouse x y evt)))))

;===============================================================================

;; make-world : PosInt -> World<%>
;; GIVEN: the value for the square toy's speed
;; RETURNS: a world with an unselected target and no toys
;; Examlpes:
;;  (make-world 10) => (new World% [x 200] [y 250] [x-click 0] [y-click 0]
;;                         [selected? false] [square-speed 10] [toys empty])
;; STRATEGY: function composition
(define (make-world square-speed)
  (new World% 
       [x CANVAS-X-CENTER]
       [y CANVAS-Y-CENTER]
       [x-click ZERO]
       [y-click ZERO]
       [selected? false]
       [square-speed square-speed]
       [toys empty]))   

;===============================================================================
;===============================================================================

;; Examples for tests:

(define init-world (make-world 10))
(define w-button-down (new World% 
                           [x CANVAS-X-CENTER]
                           [y CANVAS-Y-CENTER]
                           [x-click CANVAS-X-CENTER]
                           [y-click CANVAS-Y-CENTER]
                           [selected? true]
                           [square-speed 10]
                           [toys empty]))
(define w1-button-up (new World% 
                          [x CANVAS-X-CENTER]
                          [y CANVAS-Y-CENTER]
                          [x-click CANVAS-X-CENTER]
                          [y-click CANVAS-Y-CENTER]
                          [selected? false]
                          [square-speed 10]
                          [toys empty]))
(define w2-button-up (new World% 
                          [x 211]
                          [y CANVAS-Y-CENTER]
                          [x-click 211]
                          [y-click CANVAS-Y-CENTER]
                          [selected? false]
                          [square-speed 10]
                          [toys empty]))
(define w3-button-up (new World% 
                          [x CANVAS-X-CENTER]
                          [y CANVAS-Y-CENTER]
                          [x-click 211]
                          [y-click CANVAS-Y-CENTER]
                          [selected? false]
                          [square-speed 10]
                          [toys empty]))
(define w-drag-300-350 (new World% 
                            [x 300]
                            [y 350]
                            [x-click 300]
                            [y-click 350]
                            [selected? true]
                            [square-speed 10]
                            [toys empty]))

(define sq-t1 (new SquareToy%
                   [x CANVAS-X-CENTER]
                   [y CANVAS-Y-CENTER]
                   [square-speed 10]
                   [dir "right"]))
(define w-sq-t1 (new World% 
                     [x CANVAS-X-CENTER]
                     [y CANVAS-Y-CENTER]
                     [x-click ZERO]
                     [y-click ZERO]
                     [selected? false]
                     [square-speed 10]
                     [toys (list sq-t1)]))

(define c-t1 (new CircleToy%
                  [x CANVAS-X-CENTER]
                  [y CANVAS-Y-CENTER]
                  [tick-counter 1]
                  [color "green"]))
(define w-c-t1 (new World% 
                    [x CANVAS-X-CENTER]
                    [y CANVAS-Y-CENTER]
                    [x-click ZERO]
                    [y-click ZERO]
                    [selected? false]
                    [square-speed 10]
                    [toys (list c-t1)]))

(define w-sq-c-t1 (new World% 
                       [x CANVAS-X-CENTER]
                       [y CANVAS-Y-CENTER]
                       [x-click ZERO]
                       [y-click ZERO]
                       [selected? false]
                       [square-speed 10]
                       [toys (list c-t1 sq-t1)]))

(define sq-t1-tick (new SquareToy%
                        [x 210]
                        [y CANVAS-Y-CENTER]
                        [square-speed 10]
                        [dir "right"]))

(define c-t1-tick (new CircleToy%
                       [x CANVAS-X-CENTER]
                       [y CANVAS-Y-CENTER]
                       [tick-counter 2]
                       [color "green"]))

(define w-sq-c-t1-tick (new World% 
                            [x CANVAS-X-CENTER]
                            [y CANVAS-Y-CENTER]
                            [x-click ZERO]
                            [y-click ZERO]
                            [selected? false]
                            [square-speed 10]
                            [toys (list c-t1-tick sq-t1-tick)]))

(define i-sq-t1
  (place-image (square 40 "outline" SQ-TOY-COLOR) 200 250 
               (place-image (circle 10 "outline" "blue") 200 250 EMPTY-CANVAS)))

(define i-sq-c-t1
  (place-image (square 40 "outline" SQ-TOY-COLOR) 200 250 
               (place-image (circle 5 "solid" "green") 200 250
                            (place-image (circle 10 "outline" "blue") 200 250
                                         EMPTY-CANVAS))))

(define c-t2 (new CircleToy%
                  [x CANVAS-X-CENTER]
                  [y CANVAS-Y-CENTER]
                  [tick-counter 5]
                  [color "green"]))
(define c-t2-tick (new CircleToy%
                       [x CANVAS-X-CENTER]
                       [y CANVAS-Y-CENTER]
                       [tick-counter 1]
                       [color "red"]))

(define sq-t2 (new SquareToy%
                   [x CANVAS-X-CENTER]
                   [y CANVAS-Y-CENTER]
                   [square-speed 10]
                   [dir "left"]))
(define sq-t2-tick (new SquareToy%
                        [x 190]
                        [y CANVAS-Y-CENTER]
                        [square-speed 10]
                        [dir "left"]))

(define w-sq-c-t2 (new World% 
                       [x CANVAS-X-CENTER]
                       [y CANVAS-Y-CENTER]
                       [x-click ZERO]
                       [y-click ZERO]
                       [selected? false]
                       [square-speed 10]
                       [toys (list c-t2 sq-t2)]))
(define w-sq-c-t2-tick (new World% 
                            [x CANVAS-X-CENTER]
                            [y CANVAS-Y-CENTER]
                            [x-click ZERO]
                            [y-click ZERO]
                            [selected? false]
                            [square-speed 10]
                            [toys (list c-t2-tick sq-t2-tick)]))

(define c-t3 (new CircleToy%
                  [x CANVAS-X-CENTER]
                  [y CANVAS-Y-CENTER]
                  [tick-counter 5]
                  [color "red"]))
(define c-t3-tick (new CircleToy%
                       [x CANVAS-X-CENTER]
                       [y CANVAS-Y-CENTER]
                       [tick-counter 1]
                       [color "green"]))

(define sq-t3 (new SquareToy%
                   [x 390]
                   [y CANVAS-Y-CENTER]
                   [square-speed 10]
                   [dir "right"]))
(define sq-t3-tick (new SquareToy%
                        [x 380]
                        [y CANVAS-Y-CENTER]
                        [square-speed 10]
                        [dir "left"]))

(define sq-t4 (new SquareToy%
                   [x 10]
                   [y CANVAS-Y-CENTER]
                   [square-speed 10]
                   [dir "left"]))
(define sq-t4-tick (new SquareToy%
                        [x 20]
                        [y CANVAS-Y-CENTER]
                        [square-speed 10]
                        [dir "right"]))

(define w-sq-c-t3 (new World% 
                       [x CANVAS-X-CENTER]
                       [y CANVAS-Y-CENTER]
                       [x-click ZERO]
                       [y-click ZERO]
                       [selected? false]
                       [square-speed 10]
                       [toys (list c-t3 sq-t3 sq-t4)]))
(define w-sq-c-t3-tick (new World% 
                            [x CANVAS-X-CENTER]
                            [y CANVAS-Y-CENTER]
                            [x-click ZERO]
                            [y-click ZERO]
                            [selected? false]
                            [square-speed 10]
                            [toys (list c-t3-tick sq-t3-tick sq-t4-tick)]))

;; Functions for Tests:

;;world-equal? : World<%> World<%> -> Boolean
;;GIVEN: two worlds
;;RETURNS: true iff the two worlds are exactly identical
;;STRATEGY: HOFC
(define (world-equal? w1 w2)
  (local
    ((define t1 (send w1 get-toys))
     (define t2 (send w2 get-toys)))
    (and (equal? (reverse (rest (reverse (send w1 for-test:get-fields))))
                 (reverse (rest (reverse (send w2 for-test:get-fields)))))
         (= (length t1) (length t2))
         (toys-equal? t1 t2))))

;;toys-equal? : ListOf<Toy<%>> ListOf<Toy<%>> -> Boolean
;;GIVEN: two lists of toys
;;RETUENS: true iff the two lists are exactly identical
;;STRATEGY: HOFC
(define (toys-equal? toy-lst1 toy-lst2)
  (andmap
   (lambda (t1 t2) 
     (equal? (send t1 for-test:get-fields) 
             (send t2 for-test:get-fields)))
   toy-lst1
   toy-lst2))

;; Tests:
(begin-for-test
  (check-equal? (world-equal? (send init-world on-tick) init-world)
                true
                "the target should not be affected by the tick")
  (check-equal? (world-equal? (send init-world on-mouse 200 250 "button-down") 
                              w-button-down)
                true
                "on a button-down inside the target, the target should be made 
                 selected")
  (check-equal? (world-equal? (send init-world on-mouse 211 250 "button-down")
                              w3-button-up)
                true
                "a button-down outside target should leave the world unchanged")
  (check-equal? (world-equal? (send init-world on-mouse 200 250 "button-up")
                              w1-button-up)
                true
                "a button-up inside the target should make it unselected")
  (check-equal? (world-equal? (send init-world on-mouse 200 250 "drag") 
                              w1-button-up)
                true
                "a drag when the target is not selected should leave the world
                 unchanged")
  (check-equal? (world-equal? (send w2-button-up on-mouse 211 250 "drag") 
                              w2-button-up)
                true
                "a drag when the target is not selected should leave the world
                 unchanged")
  (check-equal? (world-equal? (send w2-button-up on-mouse 211 250 "move")
                              w2-button-up)
                true
                "any unrecognised mouse event should leave the world unchanged")
  (check-equal? (world-equal? (send w-button-down on-mouse 300 350 "drag") 
                              w-drag-300-350)
                true
                "a drag on a selected target should move the target to the new
                 mouse pointer location with smooth drag")
  (check-equal? (world-equal? (send init-world on-key "s") w-sq-t1)
                true
                "an 's' key event should create a new square toy")
  (check-equal? (world-equal? (send init-world on-key "c") w-c-t1)
                true
                "a 'c' key event should create a new circle toy")
  (check-equal? (world-equal? (send init-world on-key "a") init-world)
                true
                "any unrecognised key event should leave the world unchanged")
  (check-equal? (world-equal? (send w-sq-t1 on-key "c") w-sq-c-t1)
                true
                "a 'c' key event should create a new circle toy")
  (check-equal? (send w-sq-t1 on-draw)
                i-sq-t1
                "the scene of a world should have its elements drawn on it")
  (check-equal? (send w-sq-c-t1 on-draw)
                i-sq-c-t1
                "the scene of a world should have its elements drawn on it")
  (check-equal? (world-equal? (send w-sq-c-t1 on-tick) w-sq-c-t1-tick)
                true
                "on every tick, the square toy should move in a direction it was
                 moving before, by a specified speed")
  (check-equal? (world-equal? (send w-sq-c-t2 on-tick) w-sq-c-t2-tick)
                true
                "on every tick, the square toy should move in a direction it was
                 moving before, by a specified speed")
  (check-equal? (world-equal? (send w-sq-c-t3 on-tick) w-sq-c-t3-tick)
                true
                "on every 5th tick, the color of the circle toy should toggle"))