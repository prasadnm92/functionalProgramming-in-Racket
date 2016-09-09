;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t write repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide initial-world
         make-world
         world-after-key-event
         world-after-mouse-event
         world-balls
         make-ball
         ball-x-pos
         ball-y-pos
         ball-selected?
         world->scene
         world-on-tick
         world-after-drag)

;; MAIN FUNCTION
;;run : Any -> World
;;GIVEN: An argument, which is ignored.
;;EFFECT: runs the world at tick rate of 0.25 secs/tick.
;;RETURNS: the final state of the world.
;;         Note that the world does not respond to time passing, 
;;         so the tick rate doesn't make a difference.

(define (run num)
  (big-bang (initial-world 0)
            (on-tick world-on-tick 0.25)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)))

;---------------------------------------------------------------------------

;;DATA DEFINITIONS
(define-struct ball (x y mouse-x mouse-y selected?))
;; A Ball is a (make-ball Integer Integer Integer Integer Boolean)
;; x, y give the position of the ball's centre, on the canvas
;; mouse-x, mouse-y give the position of the mouse-pointer
;; selected? describes whether or not the ball is selected
;; TEMPLATE:
;; ball-fn : Ball -> ??
;; (define (ball-fn b)
;;   (... (ball-x b)
;;        (ball-y b)
;;        (ball-mouse-x b)
;;        (ball-mouse-y b)
;;        (ball-selected? b)))

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

(define-struct world (balls num-balls))
;; A World is a (make-world ListOfBalls Integer)
;; Interpretation:
;; balls is a list of all the balls currently on the canvas
;; num-balls is the number of balls in the world
;; TEMPLATE:
;; world-fn : World -> ??
;;  (define (world-fn w)
;;    (... (world-balls w)
;;         (world-num-balls w)))

;---------------------------------------------------------------------------

;;CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define BALL-RADIUS 20)
(define SELECTED-BALL (circle BALL-RADIUS "solid" "blue"))
(define UNSELECTED-BALL (circle BALL-RADIUS "outline" "blue"))

;...........................................................................

(define world-with-2-unselected-balls
  (make-world (list (make-ball 154 280 162 263 false)
                    (make-ball 41 256 208 133 false))
              2))

(define scene-with-2-unselected-balls
  (place-image (text "2" 12 "red") 10 10
               (place-image
                UNSELECTED-BALL 154 280
                (place-image
                 UNSELECTED-BALL 41 256 EMPTY-CANVAS))))

(define world-with-1-selected-ball
  (make-world (list (make-ball 286 92 290 87 true)) 1))

(define scene-with-1-selected-ball
  (place-image (text "1" 12 "red") 10 10
               (place-image
                SELECTED-BALL 286 92 EMPTY-CANVAS)))
;.................................................................

(define NEW-BALL (make-ball 200 150 0 0 false))

(define world-with-no-balls (make-world (list) 0))
(define world-with-ball-after-n-key (make-world (list NEW-BALL) 1))

(define BUTTON-DOWN-MOUSE-X 290)
(define BUTTON-DOWN-MOUSE-Y 87)

(define DRAG-MOUSE-X 150)
(define DRAG-MOUSE-Y 50)

(define ball1 (make-ball 342 96 150 50 false))
(define ball1-after-button-down (make-ball 342 96 290 87 false))
(define ball1-after-drag (make-ball 342 96 290 87 false))
(define ball1-after-button-up (make-ball 342 96 290 87 false))

(define ball2 (make-ball 286 92 290 87 false))
(define ball2-after-button-down (make-ball 286 92 290 87 true))
(define ball2-after-drag (make-ball 146 55 150 50 true))
(define ball2-after-button-up (make-ball 146 55 150 50 false))

(define ball3 (make-ball 40 40 150 50 false))
(define ball3-after-button-down (make-ball 40 40 290 87 false))
(define ball3-after-drag (make-ball 40 40 290 87 false))
(define ball3-after-button-up (make-ball 40 40 290 87 false))

(define ball4 (make-ball 200 200 150 50 false))
(define ball4-after-button-down (make-ball 200 200 290 87 false))
(define ball4-after-drag (make-ball 200 200 290 87 false))
(define ball4-after-button-up (make-ball 200 200 290 87 false))

(define lob1 (list ball1 ball2 ball3 ball4))
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

(define world-with-4balls 
  (make-world lob1 4))

(define world-with-balls-after-button-down 
  (make-world lob1-after-button-down 4))

(define world-with-balls-after-drag 
  (make-world lob1-after-drag 4))

(define world-with-balls-after-button-up 
  (make-world lob1-after-button-up 4))

;---------------------------------------------------------------------------

;;initial-world : Any  -> World
;;GIVEN: An argument, which is ignored.
;;RETURNS: a world with no balls.
;;EXAMPLE:
;;  (initial-world 200) => (make-world empty 0)
;;  (initial-world abc) => (make-world empty 0)
;;STRATEGY: Function Composition

(define (initial-world any)
  (make-world empty 0))

;;TESTS:
(begin-for-test
  (check-equal? (initial-world 200) (make-world empty 0)
                "iinitial world should make a world with 0 balls"))
;---------------------------------------------------------------------------

;;world->scene : World -> Scene
;;GIVEN: a World
;;RETURNS: a scene that portrays the given world
;;EXAMPLES:
;;  #(struct:world
;;    (#(struct:ball 154 280 162 263 false)
;;     #(struct:ball 41 256 208 133 false)) 12)
;;  => (place-image (text "2" 12 "red") 10 10
;;                  (place-image
;;                   UNSELECTED-BALL 154 280
;;                   (place-image
;;                    UNSELECTED-BALL 41 256 EMPTY-CANVAS)))
;;  #(struct:world 
;;    (#(struct:ball 286 92 290 87 true)) 1)
;;  => (place-image (text "1" 12 "red") 10 10
;;                  (place-image
;;                   SELECTED-BALL 286 92 EMPTY-CANVAS))
;;STRATEGY: Structural Decomposition on World : w

(define (world->scene w)
  (draw-balls (world-balls w) (world-num-balls w)))

;;draw-balls : ListOfBalls -> Scene
;;GIVEN: a list of balls
;;RETURNS: a scene that draws the balls on the canvas
;;EXAMPLES:
;;  (draw-balls (#(struct:ball 154 280 162 263 false)
;;               #(struct:ball 41 256 208 133 false))
;;              2)
;;  => (place-image (text "2" 12 "red") 10 10
;;                  (place-image
;;                   UNSELECTED-BALL 154 280
;;                   (place-image
;;                    UNSELECTED-BALL 41 256 EMPTY-CANVAS)))
;;STRATEGY: Structural Decomposition on ListOfBalls : balls

(define (draw-balls balls n)
  (cond
    [(empty? balls) (place-image (show-num-balls n)
                                 10
                                 10
                                 EMPTY-CANVAS)]
    [else (draw-ball (first balls)
                     (draw-balls (rest balls) n))]))

;;draw-ball : Ball Scene -> Scene
;;GIVEN: a Ball and a scene
;;RETURNS: a scene that draws the ball at its coordinates on the canvas
;;EXAMPLES:
;;  #(struct:ball 286 92 290 87 true)
;;   => (place-image SELECTED-BALL 286 92 EMPTY-CANVAS)

;;STRATEGY: Structural Decomposition on Ball

(define (draw-ball b s)
  (place-image (if (ball-selected? b) 
                   SELECTED-BALL 
                   UNSELECTED-BALL)
               (ball-x b)
               (ball-y b)
               s))

;;show-num-balls : Integer -> Image
;;GIVEN: an integer describing the number of balls on the canvas currently
;;RETURNS: an image of the integer in red color and font size 12
;;EXAMPLES:
;;  (show-num-balls 12) => (text "12" 12 "red")
;;  (show-num-balls 1) => (text "1" 12 "red")
;;STRATEGY: Function Composition
(define (show-num-balls n)
  (text (number->string n) 12 "red"))

;;num-of-balls : ListOfBalls -> Integer
;;GIVEN: a list of balls currently on the canvas
;;RETURNS: the number of balls in the given list
;;EXAMPLES:
;;  (num-of-balls (#(struct:ball 154 280 162 263 false)
;;                 #(struct:ball 41 256 208 133 false)) => 2
;;STRATEGY: Structural Decomposition on ListOfBalls : balls
(define (num-of-balls balls)
  (cond
    [(empty? balls) 0]
    [else (+ 1 (num-of-balls (rest balls)))]))

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
;;RETURNS: the World as it should be on the next tick, here it returns the same
;;         world
;;EXAMPLES:
;;  (world-on-tick #(struct:world
;;                   (#(struct:ball 154 280 162 263 false)
;;                    #(struct:ball 41 256 208 133 false)) 12))
;;  => (world-on-tick #(struct:world
;;                   (#(struct:ball 154 280 162 263 false)
;;                    #(struct:ball 41 256 208 133 false)) 12))

(define (world-on-tick w)
  w)

;;TESTS:
(begin-for-test
  (check-equal? (world-on-tick world-with-4balls) world-with-4balls
                "tick has no effect in this program"))
;---------------------------------------------------------------------------
;;world-after-key-event : World KeyEvent -> World
;;GIVEN: a World and a KeyEvent
;;RETURNS: the world that should follow the given world after the given
;;         key event. An "n" KeyEvent creates a new ball at the centre of the
;;         canvas. All other KeyEvents are ignored
;;EXAMPLE:
;;  (world-after-key-event #(struct:world () 0))
;;  => #(struct:world (#(struct:ball 200 150 0 0 false)) 1)
;;STRATEGY: Cases on KeyEvent : kev

(define (world-after-key-event w kev)
  (cond
    [(key=? kev "n") (world-after-n-key w)]
    [else w]))

;;world-after-n-key : World -> World
;;GIVEN: a World
;;RETURNS: the world with a new ball created at the centre of the canvas
;;EXAMPLE:
;;  (world-after-n-key #(struct:world (#(struct:ball 302 96 306 84 false)) 1))
;;  => #(struct:world (#(struct:ball 302 96 306 84 false)
;;                     #(struct:ball 200 150 0 0 false)) 2)
;;STRATEGY: Structural Deomposition on World : w

(define (world-after-n-key w)
  (make-world (append (cons (new-ball 0) empty) 
                      (world-balls w))
              (+ (num-of-balls (world-balls w)) 1)))

;;new-ball : Any -> Ball
;;GIVEN: any value
;;RETURNS: a ball at the centre of the canvas, ignores the gives value
;;EXAMPLE:
;;  (new-ball 0) => (make-ball 200 150 0 0 false)
;;  (new-ball "0") => (make-ball 200 150 0 0 false)
;;STRATEGY: Function Composition

(define (new-ball any)
  (make-ball 200 150 0 0 false))

;;TESTS:
(begin-for-test
  (check-equal? (world-after-key-event world-with-no-balls "n")
                world-with-ball-after-n-key
                "An \"n\" KeyEvent should create a new ball at the centre")
  (check-equal? (world-after-key-event world-with-4balls "q")
                world-with-4balls
                "An unrecognized KeyEvent should leave the world unchanged"))

;---------------------------------------------------------------------------

;;world-after-mouse-event : World Integer Integer MouseEvent -> World
;;GIVEN: A world, the location of a mouse event, and the mouse event itself
;;RETURNS: the world that should follow the given world after the given
;;         mouse event at the given location. The world reacts to only "drag",
;;         "button-up", "button-down" events and all other MouseEvents leave
;;         the world unchanged
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Cases on MouseEvent mev

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
;; STRATEGY: Structural Decomposition on World w

(define (world-after-button-down w mx my)
  (make-world (balls-after-button-down (world-balls w)
                                       mx
                                       my)
              (world-num-balls w)))

;;balls-after-button-down : ListOfBalls Integer Integer -> ListOfBalls
;;GIVEN: a list of balls, the x and y coordinates of the mouse
;;RETURNS: a list of balls, as it should be after a button-down event
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Structural Decomposition on ListOfBalls : balls
(define (balls-after-button-down balls mx my)
  (cond
    [(empty? balls) empty]
    [else (ball-after-button-down (first balls)
                                  (balls-after-button-down (rest balls)
                                                           mx
                                                           my)
                                  mx
                                  my)]))

;;ball-after-button-down : Ball ListOfBalls Integer Integer -> ListOfBalls
;;GIVEN: a ball, a list of remaining balls, x and y coordinates of the mouse
;;RETURNS: a list of balls, after appending the current ball to the given list
;;         of balls. Before appending, the button-down event is applied on it.
;;         i.e., if the pointer is inside the ball, it is made selected
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Structural Decomposition on Ball : b
(define (ball-after-button-down b lst mx my)
  (cons (make-ball (ball-x b)
                   (ball-y b)
                   mx
                   my
                   (in-ball? (ball-x b) (ball-y b) mx my))
        lst))

;..........................................................................

;; world-after-drag : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a drag at the given location.
;;          if the world is selected, then return a world just like the given
;;          one, except that it is now centered on the mouse position.
;; EXAMPLES:
;;     see tests below
;; STRATEGY: Structural Decomposition on World w

(define (world-after-drag w mx my)
  (make-world (balls-after-drag (world-balls w)
                                mx
                                my)
              (world-num-balls w)))

;;balls-after-drag : ListOfBalls Integer Integer -> ListOfBalls
;;GIVEN: a list of balls and the coordinates of the mouse pointer
;;RETURNS: the list of balls after a mouse-drag event
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Recurssion on ListOfBalls balls

(define (balls-after-drag balls mx my)
  (cond
    [(empty? balls) empty]
    [else (ball-after-drag (first balls)
                           mx
                           my
                           (balls-after-drag (rest balls)
                                             mx 
                                             my))]))

;;ball-after-drag : Ball Integer Integer ListOfBalls -> ListOfBalls
;;GIVEN: a ball, the coordinates of the mouse pointer and a list of balls
;;RETURNS: the list of balls after a mouse-drag event
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Structural Decomposition on Ball b

(define (ball-after-drag b mx my lst)
  (cons (if (ball-selected? b)
            (make-ball (+ mx (- (ball-x b) (ball-mouse-x b)))
                       (+ my (- (ball-y b) (ball-mouse-y b)))
                       mx
                       my
                       true)
            b)
        lst))

;..........................................................................

;; world-after-button-up : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a button-up at the given location.
;;          return a solid unselected rectangle at the given location
;; EXAMPLES:
;;     see tests below
;; STRATEGY: Structural Decomposition on World w

(define (world-after-button-up w mx my)
  (make-world (balls-after-button-up (world-balls w))
              (world-num-balls w)))

;;balls-after-button-up : ListOfBalls -> ListOfBalls
;;GIVEN: a list of balls in the world
;;RETURNS: a list of balls after a button-up event has occured
;;         i.e., all the selected balls are deselected
;;EXAMPLES:
;;     see tests below
;;STRATEGY: Structural Decomposition on ListOfBalls : balls
(define (balls-after-button-up balls)
  (cond
    [(empty? balls) empty]
    [else (ball-after-button-up (first balls)
                                (balls-after-button-up (rest balls)))]))

;;ball-after-button-up : Ball ListOfBalls -> ListOfBalls
;;GIVEN: a ball that is currently being checked with the MouseEvent and a list
;;       the rest of the balls
;;RETURNS: a list of balls with the this ball added to it, after the MouseEvent
;;         has acted on it
;;EXAMPLES:
;;  (ball-after-button-up #(struct:ball 286 92 290 87 false) empty)
;;  => (#(struct:ball 286 92 290 87 false))
;;STRATEGY: Structural Decomposition on Ball : b
(define (ball-after-button-up b lst)
  (cons (make-ball (ball-x b)
                   (ball-y b)
                   (ball-mouse-x b)
                   (ball-mouse-y b)
                   false)
        lst))

;;in-ball? : Integer Integer Integer Integer -> Boolean
;;GIVEN: a world and coordinates of the mouse pointer
;;RETURNS: true iff the given coordinate is inside the bounding box of
;;         the rectangle.
;;EXAMPLES: 
;; (in-ball? 286 92 290 87) => true
;; (in-ball? 305 153 308 128) => false
;;STRATEGY: Structural Decomposition on World w
(define (in-ball? x y mx my)
  (<= (two-point-distance x y mx my) 20))

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

;;two-point-distance : Integer Integer Integer Integer -> Real
;;GIVEN: the coordinates of two points
;;RETURNS: the distance between the two points
;;EXAMPLES:
;; (two-point-distance 286 92 290 87) => 7
;; (two-point-distance 305 153 308 128) => 26
;; (two-point-distance 200 150 119 58) => 123
;;STRATEGY: Function Composition
(define (two-point-distance x1 y1 x2 y2)
  (ceiling
   (sqrt
    (+
     (sqr
      (- x1 x2))
     (sqr
      (- y1 y2))))))

;---------------------------------------------------------------------------

;;ball-x-pos : Ball -> Integer
;;ball-y-pos : Ball -> Integer
;;GIVEN: a ball
;;RETURNS: the x or y position of its center, respectively.
;;STRATEGY: Structural Decomposition on Ball b
(define (ball-x-pos b)
  (ball-x b))

(define (ball-y-pos b)
  (ball-y b))

;;TESTS:
(begin-for-test
  (check-equal? (ball-x-pos (make-ball 200 150 0 0 false)) 200
                "ball-x-pos should return the x co-ordinate of the ball")
  (check-equal? (ball-y-pos (make-ball 200 150 0 0 false)) 150
                "ball-y-pos should return the y co-ordinate of the ball"))

;---------------------------------------------------------------------------