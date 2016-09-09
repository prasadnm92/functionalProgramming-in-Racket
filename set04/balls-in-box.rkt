;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide initial-world
         make-world
         world-after-tick
         world-after-key-event
         world-after-mouse-event
         world-balls
         world-paused?
         world-ball-speed
         make-ball
         ball-x-pos
         ball-y-pos
         ball-selected?
         ball-direction
         world->scene
         new-ball
         world-after-button-down
         world-after-drag
         world-after-button-up)

;; MAIN FUNCTION
;;run : PosInt PosReal -> World
;;GIVEN: a ball speed and a frame rate, in secs/tick
;;EFFECT: runs the world at tick rate of given frame rate
;;RETURNS: the final state of the world.

(define (run ball-speed tick-rate)
  (big-bang (initial-world ball-speed)
            (on-tick world-after-tick tick-rate)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)))

;---------------------------------------------------------------------------

;;DATA DEFINITIONS

;;A Direction is one of:
;; --right, describes that the ball is moving towards the right in the canvas
;; --left, describes that the ball is moving towards the left in the canvas
;; TEMPLATE:
;; direction-fn : Direction -> ??
;;  (define (direction-fn dir)
;;    (cond
;;      [(string=? dir "right") ...]
;;      [(string=? dir "left") ...]))

(define-struct ball (x y mouse-x mouse-y selected? direction))
;; A Ball is a (make-ball Real Real Integer Integer Boolean Direction)
;; --x, y give the position of the ball's centre, on the canvas
;; --mouse-x, mouse-y give the position of the mouse-pointer
;; --selected? describes whether or not the ball is selected
;; --direction describes one of the values as described by Direction itemization
;; TEMPLATE:
;; ball-fn : Ball -> ??
;; (define (ball-fn b)
;;   (... (ball-x b)
;;        (ball-y b)
;;        (ball-mouse-x b)
;;        (ball-mouse-y b)
;;        (ball-selected? b)
;;        (ball-direction b)))

;;A ListOfBalls is one of
;; --empty
;; --(cons Ball ListOfBalls)
;; TEMPLATE:
;; lst-fn : ListOfBalls -> ??
;; (define (lst-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (lst-fn (rest lst)))]))

(define-struct world (balls num-balls ball-speed paused?))
;; A World is a (make-world ListOfBalls NonNegInt PosInt Boolean)
;; Interpretation:
;; --balls is a list of all the balls currently on the canvas
;; --num-balls is the number of balls in the world
;; --ball-speed describes the number of pixels that the ball moves in one tick
;; --paused? is true iff the world is paused and none of the balls are moving
;; TEMPLATE:
;; world-fn : World -> ??
;;  (define (world-fn w)
;;    (... (world-balls w)
;;         (world-num-balls w)
;;         (world-ball-speed w)
;;         (world-paused? w)))

;---------------------------------------------------------------------------

;;CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define BALL-RADIUS 20)
(define SELECTED-BALL (circle BALL-RADIUS "solid" "blue"))
(define UNSELECTED-BALL (circle BALL-RADIUS "outline" "blue"))
(define RIGHT "right")
(define LEFT "left")
(define ZERO 0)
(define RIGHT-WALL (- CANVAS-WIDTH BALL-RADIUS))
(define LEFT-WALL (+ ZERO BALL-RADIUS))

;...........................................................................

(define world-with-2-unselected-balls
  (make-world (list (make-ball 154 280 162 263 false RIGHT)
                    (make-ball 41 256 208 133 false LEFT))
              2
              8
              false))

(define scene-with-2-unselected-balls
  (place-image (text "2" 12 "red") 10 10
               (place-image
                UNSELECTED-BALL 154 280
                (place-image
                 UNSELECTED-BALL 41 256 EMPTY-CANVAS))))

(define world-with-1-selected-ball
  (make-world (list (make-ball 286 92 290 87 true RIGHT)) 1 10 true))

(define scene-with-1-selected-ball
  (place-image (text "1" 12 "red") 10 10
               (place-image
                SELECTED-BALL 286 92 EMPTY-CANVAS)))
;.................................................................

(define NEW-BALL (make-ball 200 150 0 0 false RIGHT))

(define world-with-no-balls (make-world (list) 0 8 false))
(define world-with-ball-after-n-key (make-world (list NEW-BALL) 1 8 false))

(define BUTTON-DOWN-MOUSE-X 290)
(define BUTTON-DOWN-MOUSE-Y 87)

(define DRAG-MOUSE-X 150)
(define DRAG-MOUSE-Y 50)

(define ball1 (make-ball 342 96 150 50 false RIGHT))
(define ball1-after-tick (make-ball 350 96 150 50 false RIGHT))
(define ball1-after-button-down (make-ball 342 96 290 87 false RIGHT))
(define ball1-after-drag (make-ball 342 96 290 87 false RIGHT))
(define ball1-after-button-up (make-ball 342 96 290 87 false RIGHT))

(define ball2 (make-ball 286 92 290 87 false LEFT))
(define ball2-after-tick (make-ball 278 92 290 87 false LEFT))
(define ball2-after-button-down (make-ball 286 92 290 87 true LEFT))
(define ball2-after-drag (make-ball 146 55 150 50 true LEFT))
(define ball2-after-button-up (make-ball 146 55 150 50 false LEFT))

(define ball3 (make-ball 40 40 150 50 false LEFT))
(define ball3-after-tick (make-ball 32 40 150 50 false LEFT))
(define ball3-after-button-down (make-ball 40 40 290 87 false LEFT))
(define ball3-after-drag (make-ball 40 40 290 87 false LEFT))
(define ball3-after-button-up (make-ball 40 40 290 87 false LEFT))

(define ball4 (make-ball 200 200 150 50 false RIGHT))
(define ball4-after-tick (make-ball 208 200 150 50 false RIGHT))
(define ball4-after-button-down (make-ball 200 200 290 87 false RIGHT))
(define ball4-after-drag (make-ball 200 200 290 87 false RIGHT))
(define ball4-after-button-up (make-ball 200 200 290 87 false RIGHT))

(define ball5 (make-ball 375 50 30 30 false RIGHT))
(define ball5-after-tick (make-ball 380 50 30 30 false LEFT))

(define ball6 (make-ball 21 5 30 30 false LEFT))
(define ball6-after-tick (make-ball 20 5 30 30 false RIGHT))

(define ball7 (make-ball 28 32 30 30 true RIGHT))
(define ball7-after-tick (make-ball 28 32 30 30 true RIGHT))

(define lob1 (list ball1 ball2 ball3 ball4))
(define lob1-after-tick (list ball1-after-tick
                              ball2-after-tick
                              ball3-after-tick
                              ball4-after-tick))
(define lob1-after-button-down (list ball1-after-button-down
                                     ball2-after-button-down
                                     ball3-after-button-down
                                     ball4-after-button-down))
(define lob1-after-drag (list ball1-after-drag
                              ball2-after-drag
                              ball3-after-drag
                              ball4-after-drag))
(define lob1-after-button-up (list ball1-after-button-up
                                   ball2-after-button-up
                                   ball3-after-button-up 
                                   ball4-after-button-up))

(define lob2 (list ball5 ball6 ball7))
(define lob2-after-tick (list ball5-after-tick
                              ball6-after-tick
                              ball7-after-tick))

(define world-with-4balls 
  (make-world lob1 4 8 false))

(define world-with-4balls-after-pause 
  (make-world lob1 4 8 true))

(define paused-world 
  (make-world lob1 4 8 true))

(define world-with-4balls-after-tick
  (make-world lob1-after-tick 4 8 false))

(define world-with-balls-after-button-down 
  (make-world lob1-after-button-down 4 8 false))

(define world-with-balls-after-drag 
  (make-world lob1-after-drag 4 8 false))

(define world-with-balls-after-button-up 
  (make-world lob1-after-button-up 4 8 false))

(define world-before-bounce
  (make-world lob2 3 8 false))

(define world-after-bounce
  (make-world lob2-after-tick 3 8 false))


;---------------------------------------------------------------------------

;;initial-world : PosInt -> World
;;GIVEN: a ball speed
;;RETURNS: a world with no balls, , but with the property that any balls
;;         created in that world will travel at the given speed
;;EXAMPLE:
;;  (initial-world 8) => (make-world empty 0 8 false)
;;  (initial-world 100) => (make-world empty 0 100 false)
;;STRATEGY: Function Composition

(define (initial-world ball-speed)
  (make-world empty 0 ball-speed false))

;;TESTS:
(begin-for-test
  (check-equal? (initial-world 8) (make-world empty 0 8 false)
                "initial world should make a world with 0 balls and the property
                 that any ball created moves at given ball-speed"))
;---------------------------------------------------------------------------

;;world->scene : World -> Scene
;;GIVEN: a World
;;RETURNS: a scene that portrays the given world
;;EXAMPLES:
;;  #(struct:world
;;    (#(struct:ball 154 280 162 263 false RIGHT)
;;     #(struct:ball 41 256 208 133 false LEFT)) 12 8 false)
;;  => (place-image (text "2" 12 "red") 10 10
;;                  (place-image
;;                   UNSELECTED-BALL 154 280
;;                   (place-image
;;                    UNSELECTED-BALL 41 256 EMPTY-CANVAS)))
;;  #(struct:world 
;;    (#(struct:ball 286 92 290 87 true RIGHT)) 1 10 true)
;;  => (place-image (text "1" 12 "red") 10 10
;;                  (place-image
;;                   SELECTED-BALL 286 92 EMPTY-CANVAS))
;;STRATEGY: Structural Decomposition on w : World

(define (world->scene w)
  (draw-balls (world-balls w) (world-num-balls w)))

;;draw-balls : ListOfBalls -> Scene
;;GIVEN: a list of balls
;;RETURNS: a scene that draws the balls on the canvas
;;EXAMPLES:
;;  (draw-balls (#(struct:ball 154 280 162 263 false RIGHT)
;;               #(struct:ball 41 256 208 133 false LEFT))
;;              2)
;;  => (place-image (text "2" 12 "red") 10 10
;;                  (place-image
;;                   UNSELECTED-BALL 154 280
;;                   (place-image
;;                    UNSELECTED-BALL 41 256 EMPTY-CANVAS)))
;;STRATEGY: Structural Decomposition on balls : ListOfBalls

(define (draw-balls balls n)
  (foldr
   (; Ball Scene -> Scene
    ;GIVEN: a ball from the world and a scene of balls, seen so far, placed on 
    ;       the canvas
    ;RETURNS: a scene with the current ball placed in it
    lambda (ball scene-of-rest)
     (draw-ball ball scene-of-rest))
   (place-image (show-num-balls n) 10 10 EMPTY-CANVAS)
   balls))

;;draw-ball : Ball Scene -> Scene
;;GIVEN: a Ball and a scene
;;RETURNS: a scene that draws the ball at its coordinates on the canvas
;;EXAMPLES:
;;  #(struct:ball 286 92 290 87 true RIGHT)
;;   => (place-image SELECTED-BALL 286 92 EMPTY-CANVAS)

;;STRATEGY: Structural Decomposition on b : Ball

(define (draw-ball b scene-of-rest)
  (place-image (if (ball-selected? b) 
                   SELECTED-BALL 
                   UNSELECTED-BALL)
               (ball-x b)
               (ball-y b)
               scene-of-rest))

;;show-num-balls : NonNegInt -> Image
;;GIVEN: an integer describing the number of balls on the canvas currently
;;RETURNS: an image of the integer in red color and font size 12
;;EXAMPLES:
;;  (show-num-balls 12) => (text "12" 12 "red")
;;  (show-num-balls 1) => (text "1" 12 "red")
;;STRATEGY: Function Composition
(define (show-num-balls n)
  (text (number->string n) 12 "red"))

;;num-of-balls : ListOfBalls -> NonNegInt
;;GIVEN: a list of balls currently on the canvas
;;RETURNS: the number of balls in the given list
;;EXAMPLES:
;;  (num-of-balls (#(struct:ball 154 280 162 263 false)
;;                 #(struct:ball 41 256 208 133 false)) => 2
;;STRATEGY: HOFC
(define (num-of-balls balls)
  (foldr
   (;Ball NonNegInt -> NonNegInt
    ;GIVEN: the current ball being seen and the no of balls seen so far
    ;RETURNS: total no of balls in the list of balls
    lambda (ball num)
     (+ 1 num))
   ZERO
   balls))

;;TESTS:
(begin-for-test
  (check-equal? (world->scene world-with-1-selected-ball)
                scene-with-1-selected-ball
                "world->scene is not behaving properly for selected balls")
  (check-equal? (world->scene world-with-2-unselected-balls)
                scene-with-2-unselected-balls
                "world->scene is not behaving properly for unselected balls")
  (check-equal? (num-of-balls lob1) 4
                "num-of-balls should return the no of balls in list of balls"))

;---------------------------------------------------------------------------
;;world-on-tick : World -> World
;;GIVEN: a World at any instant time
;;RETURNS: the World as it should be on the next tick, if the is paused then no
;;         ball in the world should be moving
;;EXAMPLES:
;;  (world-on-tick #(struct:world
;;                   (#(struct:ball 154 280 162 263 false RIGHT)
;;                    #(struct:ball 41 256 208 133 false LEFT)) 12 8 false))
;;  => (world-on-tick #(struct:world
;;                   (#(struct:ball 162 280 162 263 false)
;;                    #(struct:ball 33 256 208 133 false)) 12 8 false))
;;STRATEGY: Structural Decomposition on w : World

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world (balls-after-tick (world-balls w) (world-ball-speed w))
                  (world-num-balls w)
                  (world-ball-speed w)
                  false)))

;;balls-after-tick : ListOfBalls PosInt -> ListOfBalls
;;GIVEN: a list of balls in the world and their ball-speed
;;RETURNS: a list just like the given, with its balls changed to reflect their
;;         status as it should be on the next tick
;;EXAMPLES:
;;  see tests
;;STRATEGY : HOFC
(define (balls-after-tick balls ball-speed)
  (map
   (; Ball -> Ball
    ;GIVEN: a ball in the world
    ;RETURNS: a ball in the world as it should be on the next tick
    lambda (ball)
     (ball-after-tick ball ball-speed))
   balls))

;;ball-after-tick : Ball PosInt -> Ball
;;GIVEN: a ball and its ball-speed
;;RETURNS: a ball just like the given, but moved by ball-speed no of pixels in
;;         its current direction when not selected
;;EXAMPLES:
;;  (ball-after-tick ball4 8) => ball4-after-tick
;;  (ball-after-tick ball7 8) => ball7-after-tick
;;STRATEGY: Structural Decomposition on ball : Ball
(define (ball-after-tick ball speed)
  (if (ball-selected? ball)
      ball
      (make-ball (update-x (ball-x ball) speed (ball-direction ball))
                 (ball-y ball)
                 (ball-mouse-x ball)
                 (ball-mouse-y ball)
                 false
                 (update-dir (ball-direction ball) (ball-x ball) speed))))

;;update-x : Real PosInt Direction -> Real
;;GIVEN: the x-coordinate, speed and direction of a ball
;;RETURNS: the x-coordinate of the ball as it should be after a tick
;;EXAMPLES:
;;   (update-x 30 8 RIGHT) => 38
;;   (update-x 22 8 LEFT) => 20
;;STRATEGY: Structural Decomposition on dir : Direction
(define (update-x x speed dir)
  (cond
    [(string=? dir RIGHT) (move-right x speed)]
    [(string=? dir LEFT) (move-left x speed)]))

;;move-right : Real PosInt -> Real
;;move-left : Real PosInt -> Real
;;GIVEN: the current x-coordinate, and the speed
;;RETURNS: the x-cordinate as it should be after a tick
;;EXAMPLES:
;;   see tests
;;STRATEGY: Function Composition
(define (move-right x speed)
  (min (+ x speed) RIGHT-WALL))

(define (move-left x speed)
  (max (- x speed) LEFT-WALL))

;;update-dir : Direction Real -> Direction
;;GIVEN: the direction and x-coordinate of the ball
;;RETURNS: the direction as it should be on the next tick
;;EXAMPLES:
;;   see tests below
;;STRATEGY: Structural Decomposition on dir : Direction
(define (update-dir dir x speed)
  (cond
    [(string=? dir RIGHT) (if (>= (+ x speed) RIGHT-WALL)
                               LEFT
                               RIGHT)]
    [(string=? dir LEFT) (if (<= (- x speed) LEFT-WALL)
                               RIGHT
                               LEFT)]))

;;TESTS:
(begin-for-test
  (check-equal? (world-after-tick world-with-4balls)
                world-with-4balls-after-tick
                "after 1 tick, the balls should move at ball-speed in the same
                 direction as it was before, unless it hits a boundary wall,
                 in which case it should bounce back")
  (check-equal? (world-after-tick paused-world)
                paused-world
                "a paused world should stay unchanged")
  (check-equal? (world-after-tick world-before-bounce)
                world-after-bounce
                "when either of the boundary walls are approached, the balls
                 must bounce back"))
;---------------------------------------------------------------------------
;;world-after-key-event : World KeyEvent -> World
;;GIVEN: a World and a KeyEvent
;;RETURNS: the world that should follow the given world after the given
;;         key event. An "n" KeyEvent creates a new ball at the centre of the
;;         canvas. All other KeyEvents are ignored
;;EXAMPLE:
;;  (world-after-key-event #(struct:world () 0))
;;  => #(struct:world (#(struct:ball 200 150 0 0 false)) 1)
;;STRATEGY: Cases on kev : KeyEvent

(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ") (world-on-pause w)]
    [(key=? kev "n") (world-after-n-key w)]
    [else w]))

;;world-on-pause : World -> World
;;GIVEN: a world
;;RETURNS: a world, just like the given, with its paused status toggled
;;EXAMPLES:
;;   see tests below
;;STRATEGY: Structural Decomposition on w : World

(define (world-on-pause w)
  (make-world (world-balls w)
              (world-num-balls w)
              (world-ball-speed w)
              (not (world-paused? w))))

;;world-after-n-key : World -> World
;;GIVEN: a World
;;RETURNS: the world with a new ball created at the centre of the canvas
;;EXAMPLE:
;;  (world-after-n-key #(struct:world (#(struct:ball 302 96 306 84 false)) 1))
;;  => #(struct:world (#(struct:ball 302 96 306 84 false)
;;                     #(struct:ball 200 150 0 0 false)) 2)
;;STRATEGY: Structural Deomposition on w : World

(define (world-after-n-key w)
  (make-world (cons (new-ball 0) (world-balls w))
              (+ (num-of-balls (world-balls w)) 1)
              (world-ball-speed w)
              (world-paused? w)))

;;new-ball : Any -> Ball
;;GIVEN: any value
;;RETURNS: a ball at the centre of the canvas, ignores the gives value
;;EXAMPLE:
;;  (new-ball 0) => (make-ball 200 150 0 0 false)
;;  (new-ball "0") => (make-ball 200 150 0 0 false)
;;STRATEGY: Function Composition

(define (new-ball any)
  (make-ball 200 150 0 0 false RIGHT))

;;TESTS:
(begin-for-test
  (check-equal? (world-after-key-event world-with-no-balls "n")
                world-with-ball-after-n-key
                "An \"n\" KeyEvent should create a new ball at the centre")
  (check-equal? (world-after-key-event world-with-4balls "q")
                world-with-4balls
                "An unrecognized KeyEvent should leave the world unchanged")
  (check-equal? (world-after-key-event world-with-4balls " ")
                world-with-4balls-after-pause
                "A \" \" (pause) KeyEvent should toggle the pause state of all
                the balls in the world"))

;---------------------------------------------------------------------------

;;world-after-mouse-event : World Integer Integer MouseEvent -> World
;;GIVEN: A world, the location of a mouse event, and the mouse event itself
;;RETURNS: the world that should follow the given world after the given
;;         mouse event at the given location. The world reacts to only "drag",
;;         "button-up", "button-down" events and all other MouseEvents leave
;;         the world unchanged
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Cases on mev : MouseEvent

(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

;; world-after-button-down : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a button-down at the given location.
;;          if the button-down is inside a ball, make the ball selected
;; EXAMPLES:
;;     see tests below
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-button-down w mx my)
  (make-world (balls-after-button-down (world-balls w)
                                       mx
                                       my)
              (world-num-balls w)
              (world-ball-speed w)
              (world-paused? w)))

;;balls-after-button-down : ListOfBalls Integer Integer -> ListOfBalls
;;GIVEN: a list of balls, the x and y coordinates of the mouse
;;RETURNS: a list of balls, as it should be after a button-down event
;;EXAMPLES:
;;     see tests below
;;STRATEGY: HOFC

(define (balls-after-button-down balls mx my)
  (map
   (; Ball -> Ball
    ;GIVEN: a ball in the world
    ;RETURNS: a ball just like the give, after a button-down event is applied
    lambda (ball)
     (ball-after-button-down ball mx my))
   balls))

;;ball-after-button-down : Ball Integer Integer -> Ball
;;GIVEN: a ball, a list of remaining balls, x and y coordinates of the mouse
;;RETURNS: a list of balls, after appending the current ball to the given list
;;         of balls. Before appending, the button-down event is applied on it.
;;         i.e., if the pointer is inside the ball, it is made selected
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Structural Decomposition on b : Ball
(define (ball-after-button-down b mx my)
  (make-ball (ball-x b)
             (ball-y b)
             mx
             my
             (in-ball? (ball-x b) (ball-y b) mx my)
             (ball-direction b)))

;..........................................................................

;; world-after-drag : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a drag at the given location.
;;          if the world is selected, then return a world just like the given
;;          one, except that it is now centered on the mouse position.
;; EXAMPLES:
;;     see tests below
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-drag w mx my)
  (make-world (balls-after-drag (world-balls w)
                                mx
                                my)
              (world-num-balls w)
              (world-ball-speed w)
              (world-paused? w)))

;;balls-after-drag : ListOfBalls Integer Integer -> ListOfBalls
;;GIVEN: a list of balls and the coordinates of the mouse pointer
;;RETURNS: the list of balls after a mouse-drag event
;;EXAMPLES:
;;     see tests below
;;STRATEGY: HOFC

(define (balls-after-drag balls mx my)
  (map
   (; Ball -> Ball
    ;GIVEN: a ball in the world
    ;RETURNS: a ball just like the give, after a drag event is applied on it
    lambda (ball)
     (ball-after-drag ball mx my))
   balls))

;;ball-after-drag : Ball Integer Integer -> Ball
;;GIVEN: a ball, the coordinates of the mouse pointer and a list of balls
;;RETURNS: the list of balls after a mouse-drag event
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Structural Decomposition on b : Ball

(define (ball-after-drag b mx my)
  (if (ball-selected? b)
      (make-ball (+ mx (- (ball-x b) (ball-mouse-x b)))
                 (+ my (- (ball-y b) (ball-mouse-y b)))
                 mx
                 my
                 true
                 (ball-direction b))
      b))

;..........................................................................

;; world-after-button-up : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a button-up at the given location.
;;          return a solid unselected rectangle at the given location
;; EXAMPLES:
;;     see tests below
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-button-up w mx my)
  (make-world (balls-after-button-up (world-balls w))
              (world-num-balls w)
              (world-ball-speed w)
              (world-paused? w)))

;;balls-after-button-up : ListOfBalls -> ListOfBalls
;;GIVEN: a list of balls in the world
;;RETURNS: a list of balls after a button-up event has occured
;;         i.e., all the selected balls are deselected
;;EXAMPLES:
;;     see tests below
;;STRATEGY: HOFC

(define (balls-after-button-up balls)
  (map
   (; Ball -> Ball
    ;GIVEN: a ball in the world
    ;;RETURNS: a ball just like the give, after a button-up event is applied
    lambda (ball)
     (ball-after-button-up ball))
   balls))

;;ball-after-button-up : Ball -> Ball
;;GIVEN: a ball that is currently being checked with the MouseEvent and a list
;;       the rest of the balls
;;RETURNS: a list of balls with the this ball added to it, after the MouseEvent
;;         has acted on it
;;EXAMPLES:
;;  (ball-after-button-up #(struct:ball 286 92 290 87 false) empty)
;;  => (#(struct:ball 286 92 290 87 false))
;;STRATEGY: Structural Decomposition on b : Ball
(define (ball-after-button-up b)
  (make-ball (check-x (ball-x b))
             (ball-y b)
             (ball-mouse-x b) (ball-mouse-y b) false
             (ball-direction b)))

;;check-x : Real -> Real
;;GIVEN: the current x-coordinate of the center of the ball
;;RETURNS: the x-coordinate of the centre of the ball after the mouse button is
;;         released
;;EXAMPLES:
;;   (check-x 15) => 20
;;   (check-x 410) => 380
;;STRATEGY: Function Composition
(define (check-x x)
  (max (min x RIGHT-WALL)
       LEFT-WALL))

;;in-ball? : Real Real Integer Integer -> Boolean
;;GIVEN: a world and coordinates of the mouse pointer
;;RETURNS: true iff the given coordinate is inside the bounding box of
;;         the rectangle.
;;EXAMPLES: 
;; (in-ball? 286 92 290 87) => true
;; (in-ball? 305 153 308 128) => false
;;STRATEGY: Function Composition
(define (in-ball? x y mx my)
  (<= (two-point-distance x y mx my) BALL-RADIUS))

;;TESTS:
(begin-for-test
  (check-equal? (world-after-mouse-event world-with-4balls 
                                         BUTTON-DOWN-MOUSE-X
                                         BUTTON-DOWN-MOUSE-Y
                                         "button-down")
                world-with-balls-after-button-down
                "A button-down event will make a ball selected iff the pointer
                 is inside the ball")
  (check-equal? (world-after-mouse-event world-with-balls-after-button-down 
                                         DRAG-MOUSE-X 
                                         DRAG-MOUSE-Y
                                         "drag")
                world-with-balls-after-drag
                "A drag event will move a ball iff it is selected")
  (check-equal? (world-after-mouse-event world-with-balls-after-drag 
                                         DRAG-MOUSE-X 
                                         DRAG-MOUSE-Y 
                                         "button-up")
                world-with-balls-after-button-up
                "A button-up event will make a ball unselected iff the pointer
                 is inside the ball")
  (check-equal? (world-after-mouse-event world-with-4balls 150 50 "enter")
                world-with-4balls
                "An unrecognized mouse-event should leave the world unchanged"))

;............................................................................

;;two-point-distance : Real Real Integer Integer -> Real
;;GIVEN: the coordinates of two points
;;RETURNS: the distance between the two points
;;EXAMPLES:
;; (two-point-distance 286 92 290 87) => 7
;; (two-point-distance 305 153 308 128) => 26
;; (two-point-distance 200 150 119 58) => 123
;;STRATEGY: Function Composition
(define (two-point-distance x1 y1 x2 y2)
  (ceiling (sqrt (+ (sqr (- x1 x2))
                    (sqr (- y1 y2))))))

;---------------------------------------------------------------------------

;;ball-x-pos : Ball -> Real
;;ball-y-pos : Ball -> Real
;;GIVEN: a ball
;;RETURNS: the x or y position of its center, respectively.
;;STRATEGY: Structural Decomposition on b : Ball
(define (ball-x-pos b)
  (ball-x b))

(define (ball-y-pos b)
  (ball-y b))

;;TESTS:
(begin-for-test
  (check-equal? (ball-x-pos (make-ball 200 150 0 0 false RIGHT)) 200
                "ball-x-pos should return the x co-ordinate of the ball")
  (check-equal? (ball-y-pos (make-ball 200 150 0 0 false LEFT)) 150
                "ball-y-pos should return the y co-ordinate of the ball"))

;---------------------------------------------------------------------------