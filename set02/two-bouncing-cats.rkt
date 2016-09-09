;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname two-bouncing-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; start with (run 0)

(provide
 initial-world
 make-world
 world-cat1
 world-cat2
 world-paused?
 make-cat
 cat-x-pos
 cat-y-pos
 cat-selected?
 cat-north?
 cat-south?
 cat-east?
 cat-west?
 world-after-tick
 world-after-mouse-event
 world-after-key-event)

;; MAIN FUNCTION.
;; run : Number -> World
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world
(define (run initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world->scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;........................................................................

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimensions of the cat
(define CAT-WIDTH (image-width  CAT-IMAGE))
(define CAT-HEIGHT (image-height CAT-IMAGE))
(define HALF-CAT-WIDTH  (round (/ CAT-WIDTH 2)))
(define HALF-CAT-HEIGHT (round (/ CAT-HEIGHT 2)))

;; buffer for the bouncing effect
(define BUFFER (/ CATSPEED 4))

;; limits of the cat's movements
(define CAT-NORTH-LIMIT (+ 0 HALF-CAT-HEIGHT BUFFER))
(define CAT-SOUTH-LIMIT (- CANVAS-HEIGHT HALF-CAT-HEIGHT BUFFER))
(define CAT-EAST-LIMIT (- CANVAS-WIDTH HALF-CAT-WIDTH BUFFER))
(define CAT-WEST-LIMIT (+ 0 HALF-CAT-WIDTH BUFFER))

;........................................................................

;;; DATA DEFINITIONS

;;Direction is one of:
;; --"north" means the cat is currently moving in the north direction 
;; --"south" means the cat is currently moving in the south direction 
;; --"east" means the cat is currently moving in the east direction 
;; --"west" means the cat is currently moving in the west direction
;;TEMPLATE:
; (define (direction-fn d)
;   (cond
;     [(string=? d "north") (...)]
;     [(string=? d "south") (...)]
;     [(string=? d "east") (...)]
;     [(string=? d "west") (...)]))

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean Direction)
;; Interpretation:
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused
;; TEMPLATE:
;; world-fn : World -> ??
; (define (world-fn w)
;   (... (world-cat1 w) 
;        (world-cat2 w) 
;        (world-paused? w)))

(define-struct cat (x-pos y-pos selected? direction))
;; A Cat is a (make-cat Real Real Boolean)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.
;; direction describes the direction the cat is currently moving in.
;; TEMPLATE:
;; cat-fn : Cat -> ??
; (define (cat-fn c)
;  (... (cat-x-pos c)
;       (cat-y-pos c)
;       (cat-selected? c)
;       (cat-direction c)))

;........................................................................

;;Constants for testing

(define unselected-cat-at-300-200 (make-cat 300 200 false "north"))
(define unselected-cat-at-150-100 (make-cat 150 100 false "east"))
(define selected-cat-at-150-100 (make-cat 150 100 true "east"))
(define selected-cat-at-250-200 (make-cat 250 200 true "east"))
(define selected-cat-at-160-110 (make-cat 160 110 true "east"))
(define unselected-cat-at-160-110 (make-cat 160 110 false "east"))
(define selected-cat-at-20-200 (make-cat 20 200 true "east"))
(define cat-after-drag-to-20-200 (make-cat CAT-WEST-LIMIT
                                           200 
                                           false
                                           "west"))
(define selected-cat-at-160-10 (make-cat 160 10 true "east"))
(define cat-after-drag-to-160-10 (make-cat 160
                                           CAT-NORTH-LIMIT
                                           false
                                           "west"))

(define selected-cat-at-150-100-north (make-cat 150 100 true "north"))
(define selected-cat-at-150-100-south (make-cat 150 100 true "south"))
(define selected-cat-at-150-100-east (make-cat 150 100 true "east"))
(define selected-cat-at-150-100-west (make-cat 150 100 true "west"))
(define unselected-cat-at-150-100-south (make-cat 300 100 false "south"))
(define unselected-cat-at-150-108-south (make-cat 300 108 false "south"))
;========================================================================
(define PAUSE-KEY " ")
(define UP-KEY "up")
(define DOWN-KEY "down")
(define RIGHT-KEY "right")
(define LEFT-KEY "left")
;========================================================================
(define unpaused-world-with-2-unselected-cats
  (make-world unselected-cat-at-150-100
              unselected-cat-at-300-200
              false))

(define paused-world-with-2-unselected-cats
  (make-world unselected-cat-at-150-100
              unselected-cat-at-300-200
              true))

(define unpaused-world-with-cats-one-north
  (make-world unselected-cat-at-300-200
              selected-cat-at-150-100-north
              false))

(define unpaused-world-with-cats-one-south
  (make-world unselected-cat-at-300-200
              selected-cat-at-150-100-south
              false))

(define unpaused-world-with-cats-one-east
  (make-world unselected-cat-at-300-200
              selected-cat-at-150-100-east
              false))

(define unpaused-world-with-cats-one-west
  (make-world unselected-cat-at-300-200
              selected-cat-at-150-100-west
              false))

(define unpaused-world-with-one-cat-selected-at-100
  (make-world selected-cat-at-150-100-south
              unselected-cat-at-150-100-south
              false))

(define unpaused-world-with-one-cat-selected-at-108
  (make-world selected-cat-at-150-100-south
              unselected-cat-at-150-108-south
              false))

;========================================================================
(define world-at-20
  (make-world (make-cat 150 20 false "south")
              (make-cat 300 20 false "south")
              false))

(define image-of-world-at-20
  (place-image CAT-IMAGE 150 20
               (place-image CAT-IMAGE 300 20
                            EMPTY-CANVAS)))

;........................................................................

;; world-after-tick : World -> World
;; GIVEN: a World w
;; RETURNS: the world that should follow w after a tick.
;; EXAMPLES:
;;   (world-after-tick (make-world
;;                      (make-cat 150 300 false "south")
;;                      (make-cat 300 300 false "north")
;;                      false)) => (make-world
;;                                 (make-cat 150 308 false "south")
;;                                 (make-cat 300 292 false "north")
;;                                 false)
;; STRATEGY: Structural Decomposition on World w
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (cat-after-tick (world-cat1 w))
       (cat-after-tick (world-cat2 w))
       (world-paused? w))))

;; cat-after-tick : Cat -> Cat
;; GIVEN: a cat c
;; RETURNS: the state of the given cat after a tick if it were in an
;;          unpaused world.
;; EXAMPLES: 
;;   (cat-after-tick (make-cat 150 300 false "east"))
;;          => (make-cat 158 300 false "east")
;; STRATEGY: Structural Decomposition on Cat c
(define (cat-after-tick c)
  (cat-after-tick-helper (cat-x-pos c)
                         (cat-y-pos c)
                         (cat-selected? c)
                         (cat-direction c)))

;; cat-after-tick-helper : Real Real Boolean Direction -> Cat
;; GIVEN: a position, a value for selected? and a direction
;; RETURNS: the cat that should follow one in the given position in an
;;          unpaused world 
;; EXAMPLES:
;;   (cat-after-tick-helper (make-cat 150 300 true "east"))
;;          => (make-cat 150 300 true "east")
;; STRATEGY: Function Composition
(define (cat-after-tick-helper x y selected? dir)
  (if selected?
      (make-cat x y selected? dir)
      (cat-after-tick-helper2 x 
                              (max CAT-NORTH-LIMIT
                                   (min y CAT-SOUTH-LIMIT))
                              dir)))

;; cat-after-tick-helper2 : Real Real Direction -> Cat
;; GIVEN: a position, a value for selected? and a direction
;; RETURNS: the cat that should follow one in the given position in an
;;          unpaused world 
;; EXAMPLES:
;;  (cat-after-tick-helper2 150 300 "west") => (make-cat 150 292 false "west")
;; STRATEGY: Structural Decomposition on Direction dir
(define (cat-after-tick-helper2 x y d)
  (cond
    [(north? d) (cat-after-tick-north x y)]
    [(south? d) (cat-after-tick-south x y)]
    [(east? d) (cat-after-tick-east x y)]
    [(west? d) (cat-after-tick-west x y)]))

;; north? : Direction -> Boolean
;; south? : Direction -> Boolean
;; east? : Direction -> Boolean
;; west? : Direction -> Boolean
;; GIVEN: a direction
;; RETURNS: true iff the direction is same as that being compared
;; EXAMPLES:
;;   (north? "north") => true
;;   (south? "east") => false
;; STRATEGY: Function Composition
(define (north? d)
  (string=? d "north"))

(define (south? d)
  (string=? d "south"))

(define (east? d)
  (string=? d "east"))

(define (west? d)
  (string=? d "west"))

;========================================================================

;; cat-after-tick-north : Real Real -> Cat
;; cat-after-tick-south : Real Real -> Cat
;; cat-after-tick-east : Real Real -> Cat
;; cat-after-tick-west : Real Real -> Cat
;; GIVEN: a position of the Cat, if it is selected or not and
;;        its direction of movement
;; RETURNS: a Cat as it should be on the next tick
;; EXAMPLES:
;;   (cat-after-tick-south 300 200) 
;;                  => (make-cat 300 208 false "south")
;;   (cat-after-tick-west CAT-WEST-LIMIT 250) 
;;                  => (make-cat CAT-WEST-LIMIT 250 "east")
;; STRATEGY: Function Composition
(define (cat-after-tick-north x y)
  (if (cat-at-top? x (- y CATSPEED))
      (make-cat x
                CAT-NORTH-LIMIT
                false
                "south")
      (make-cat x
                (- y CATSPEED)
                false
                "north")))

(define (cat-after-tick-south x y)
  (if (cat-at-bottom? x (+ y CATSPEED))
      (make-cat x
                CAT-SOUTH-LIMIT
                false
                "north")
      (make-cat x
                (+ y CATSPEED) 
                false
                "south")))

(define (cat-after-tick-east x y)
  (if (cat-at-right? (+ x CATSPEED) y)
      (make-cat CAT-EAST-LIMIT
                y
                false
                "west")
      (make-cat (+ x CATSPEED)
                y
                false
                "east")))

(define (cat-after-tick-west x y)
  (if (cat-at-left? (- x CATSPEED) y)
      (make-cat CAT-WEST-LIMIT
                y
                false
                "east")
      (make-cat (- x CATSPEED)
                y
                false
                "west")))
(begin-for-test
  (check-equal? (cat-after-tick-helper2 250 200 "west")
                (make-cat 242 200 false "west")
                "")
  (check-equal? (cat-after-tick-east 250 CAT-SOUTH-LIMIT)
                (make-cat 258 CAT-SOUTH-LIMIT false "east")
                "")
  (check-equal? (cat-after-tick-helper2 CAT-EAST-LIMIT 200 "east")
                (make-cat CAT-EAST-LIMIT 200 false "west")
                "")
  (check-equal? (cat-after-tick-helper2 250 CAT-NORTH-LIMIT "north")
                (make-cat 250 CAT-NORTH-LIMIT false "south")
                "")
  (check-equal? (cat-after-tick-north 250 200)
                (make-cat 250 192 false "north")
                "")
  (check-equal? (cat-after-tick-south 250 CAT-NORTH-LIMIT) 
                (make-cat 250 (+ CAT-NORTH-LIMIT 8) false "south")
                "")
  (check-equal? (cat-after-tick-helper2 250 CAT-SOUTH-LIMIT "south")
                (make-cat 250 CAT-SOUTH-LIMIT false "north")
                "")
  (check-equal? (cat-after-tick-west 0 300) 
                (make-cat CAT-WEST-LIMIT 300 false "east")
                ""))

;; cat-at-bottom? : Real Real -> Boolean
;; cat-at-top? : Real Real -> Boolean
;; cat-at-right? : Real Real -> Boolean
;; cat-at-left? : Real Real -> Boolean
;; GIVEN: the coordinates of the centre of the cat
;; RETURNS: true iff the cat has reached any of the borders
;; EXAMPLES:
;;   (cat-at-top? 150 60) => true
;;   (cat-at-right? 300 200) => false
;; STRATEGY: Function Composition

(define (cat-at-bottom? x y)
  (>= y CAT-SOUTH-LIMIT))

(define (cat-at-top? x y)
  (<= y CAT-NORTH-LIMIT))

(define (cat-at-right? x y)
  (>= x CAT-EAST-LIMIT))

(define (cat-at-left? x y)
  (<= x CAT-WEST-LIMIT))

(check-equal? (cat-at-top? 150 150) false "150>60.5 => not reached top wall")
(check-equal? (cat-at-right? 435 200) true "435>410.5 => reached right wall")
(check-equal? (cat-at-left? 435 200) false "435>39.5 => not reached left wall")

;; TESTS:
(begin-for-test
  
  (check-equal? (world-after-tick paused-world-with-2-unselected-cats)
                paused-world-with-2-unselected-cats
                "when paused, the world should stay still")
  (check-equal? (world-after-tick unpaused-world-with-one-cat-selected-at-100)
                unpaused-world-with-one-cat-selected-at-108
                "the cat should move at CATSPEED, when it is not selected")) 

;........................................................................

;; world->scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLES: 
;;   refer the test case below
;; STRATEGY: Structural Decomposition World w
(define (world->scene w)
  (place-cat
   (world-cat1 w)
   (place-cat
    (world-cat2 w)
    EMPTY-CANVAS)))

;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
;; STRATEGY: Function Composition
(define (place-cat c s)
  (place-image
   CAT-IMAGE
   (cat-x-pos c) (cat-y-pos c)
   s))

;; TESTS:
(begin-for-test
  (check-equal? (world->scene world-at-20)
                image-of-world-at-20
                "world->scene is not behaving properly"))

;........................................................................

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below 
;; STRATEGY: Cases on KeyEvent kev

(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ") (world-with-paused-toggled w)]
    [(key=? kev "up") (make-world (cat-on-key-up (world-cat1 w))
                                  (cat-on-key-up (world-cat2 w))
                                  (world-paused? w))]
    [(key=? kev "down") (make-world (cat-on-key-down (world-cat1 w))
                                    (cat-on-key-down (world-cat2 w))
                                    (world-paused? w))]
    [(key=? kev "right") (make-world (cat-on-key-right (world-cat1 w))
                                     (cat-on-key-right (world-cat2 w))
                                     (world-paused? w))]
    [(key=? kev "left") (make-world (cat-on-key-left (world-cat1 w))
                                    (cat-on-key-left (world-cat2 w))
                                    (world-paused? w))]
    [else w]))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Structural Decomposition on World w

(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))

;; cat-on-key-up : Cat -> Cat
;; cat-on-key-down : Cat -> Cat
;; cat-on-key-right : Cat -> Cat
;; cat-on-key-left : Cat -> Cat
;; GIVEN: a cat
;; RETURNS: a cat after the key event has acted on it
;; EXAMPLES:
;;   (cat-on-key-down (make-cat 200 300 true "west")) 
;;          => (make-cat 200 300 true "south")
;;   (cat-on-key-right (make-cat 200 300 false "west")) 
;;          => (make-cat 200 300 false "west")
;; STRATEGY: Function Composition
(define (cat-on-key-up c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c)
                (cat-y-pos c)
                true
                "north")
      c))

(define (cat-on-key-down c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c)
                (cat-y-pos c)
                true
                "south")
      c))

(define (cat-on-key-right c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c)
                (cat-y-pos c)
                true
                "east")
      c))

(define (cat-on-key-left c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c)
                (cat-y-pos c)
                true
                "west")
      c))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-key-event 
                 unpaused-world-with-2-unselected-cats
                 PAUSE-KEY)
                paused-world-with-2-unselected-cats
                "on a Pause event(space bar), the world should pause")
  (check-equal? (world-after-key-event 
                 unpaused-world-with-cats-one-north
                 RIGHT-KEY)
                unpaused-world-with-cats-one-east
                "on a Right key-event, the selected cat should
                 change direction to east")
  (check-equal? (world-after-key-event 
                 unpaused-world-with-cats-one-south
                 LEFT-KEY)
                unpaused-world-with-cats-one-west
                "on a Left key-event, the selected cat should 
                 change direction to west")
  (check-equal? (world-after-key-event 
                 unpaused-world-with-cats-one-west
                 UP-KEY)
                unpaused-world-with-cats-one-north
                "on an Up key-event, the selected cat should 
                 change direction to north")
  (check-equal? (world-after-key-event 
                 unpaused-world-with-cats-one-east
                 DOWN-KEY)
                unpaused-world-with-cats-one-south
                "on a Down key-event, the selected cat should 
                 change direction to south")
  (check-equal? (world-after-key-event 
                 unpaused-world-with-2-unselected-cats
                 "q")
                unpaused-world-with-2-unselected-cats
                "any other key event should leave the world unchanged"))


;........................................................................

;; world-after-mouse-event : World Real Real MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; EXAMPLES: see tests below
;; STRATEGY: Function Composition
(define (world-after-mouse-event w mx my mev)
  (make-world
   (cat-after-mouse-event (world-cat1 w) mx my mev)
   (cat-after-mouse-event (world-cat2 w) mx my mev)
   (world-paused? w)))

;; cat-after-mouse-event : Cat Real Real MouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; EXAMPLES:
;;   (cat-after-mouse-event (make-cat 150 200 false "south") 
;;                          160 
;;                          210
;;                          "button-down")
;;            => (make-cat 150 200 true "south")
;; STRATEGY: Cases on MouseEvents
(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (cat-after-button-down c mx my)]
    [(mouse=? mev "drag") (cat-after-drag c mx my)]
    [(mouse=? mev "button-up") (cat-after-button-up c mx my)]
    [else c]))

;; cat-after-button-down : Cat Real Real -> Cat
;; RETURNS: the cat following a button-down at the given location.
;; EXAMPLES: see tests below
;; STRATEGY: Structural Decomposition on Cat c
(define (cat-after-button-down c mx my)
  (if (in-cat? c mx my)
      (make-cat (cat-x-pos c) (cat-y-pos c) true (cat-direction c))
      c))

;; cat-after-drag : Cat Real Real -> Cat
;; RETURNS: the cat following a drag at the given location
;; EXAMPLES: see tests below
;; STRATEGY: Structural Decomposition on Cat c
(define (cat-after-drag c mx my)
  (if (cat-selected? c)
      (make-cat mx
                my
                true 
                (cat-direction c))
      c))

;; cat-after-button-up : Cat Real Real -> Cat
;; RETURNS: the cat following a button-up at the given location
;; EXAMPLES: see tests below
;; STRATEGY: Structural Decomposition on Cat c
(define (cat-after-button-up c mx my)
  (if (cat-selected? c)
      (make-cat (cat-x-pos-helper (cat-x-pos c))
                (cat-y-pos-helper (cat-y-pos c))
                false
                (cat-direction-helper (cat-x-pos c)
                                      (cat-y-pos c)
                                      (cat-direction c)
                                      mx
                                      my))
      c))

;; cat-x-pos-helper : Real -> Real
;; cat-y-pos-helper : Real -> Real
;; GIVEN: the x or y position of the cat image
;; RETURNS: the x or y position of the cat image after a button-up event
;;          If, image is dragged outside the canvas, the image should
;;          start back from that limit
;; EXAMPLES: see tests below
;; STRATEGY: Function Composition
(define (cat-x-pos-helper x)
  (if (<= x CAT-WEST-LIMIT)
      CAT-WEST-LIMIT
      (if (>= x CAT-EAST-LIMIT) CAT-EAST-LIMIT x)))

(define (cat-y-pos-helper y)
  (if (<= y CAT-NORTH-LIMIT)
      CAT-NORTH-LIMIT
      (if (>= y CAT-SOUTH-LIMIT) CAT-SOUTH-LIMIT y)))

;; cat-direction-helper : Real Real Direction Real Real -> Direction
;; GIVEN: a cat, the coordinates of the mouse pointer
;; RETURNS: the direction of the cat that should be, after a button-up event
;; EXAMPLES: see tests below
;; STRATEGY: Structural Decomposition on Cat c
(define (cat-direction-helper x y d mx my)
  (if (and
       (>= y CAT-NORTH-LIMIT)
       (<= y CAT-SOUTH-LIMIT)
       (<= x CAT-EAST-LIMIT)
       (>= x CAT-WEST-LIMIT))
      d
      (change-direction d)))

;; change-direction : Direction -> Direction
;; GIVEN: the direction of a cat before button-up event
;; RETURNS: the direction of a cat after button-up event
;; EXAMPLES: see tests below
;; STRATEGY: Structural Decomposition on Direction d
(define (change-direction d)
  (cond
    [(north? d) "south"]
    [(south? d) "north"]
    [(west? d) "east"]
    [(east? d) "west"]))


;; TESTS:
(begin-for-test
  (check-equal? (change-direction "north") "south" 
                "north should change to south")
  (check-equal? (change-direction "south") "north"
                "south should change to north")
  (check-equal? (change-direction "west") "east"
                "west should change to east")
  (check-equal? (cat-y-pos-helper 380) CAT-SOUTH-LIMIT
                "when the centre of cat crosses its southern limit, it should
               jump back to the southern limit and be tangent to the border")
  
  (check-equal? (cat-after-button-up (make-cat 10 10 true "south") 10 10)
                (make-cat CAT-WEST-LIMIT CAT-NORTH-LIMIT false "north")
                "")
  (check-equal? (cat-after-button-up (make-cat 440 390 true "east") 440 390)
                (make-cat CAT-EAST-LIMIT CAT-SOUTH-LIMIT false "west")
                "")
  (check-equal? (cat-after-button-up (make-cat 445 150 true "north")
                                     445
                                     150)
                (make-cat CAT-EAST-LIMIT 150 false "south")
                "when a cat is dropped ouside east limit, it should spring
                 back inside, tangent to the east wall")
  (check-equal? (cat-after-button-up selected-cat-at-160-10
                                     160
                                     10)
                cat-after-drag-to-160-10
                "on button-up, a cat dragged outside the upper/lower border of  
                canvas that is travelling horizontally should spring back in 
                and change direction by 180 degree")
  (check-equal? (world-after-mouse-event
                 (make-world selected-cat-at-20-200
                             unselected-cat-at-300-200
                             false)
                 20
                 20
                 "button-up")
                (make-world cat-after-drag-to-20-200
                            unselected-cat-at-300-200
                            false)
                "on button-up, a cat dragged outside the canvas should
                 spring back inside")
  (check-equal? (world-after-mouse-event
                 (make-world selected-cat-at-160-110
                             unselected-cat-at-300-200
                             false)
                 20
                 200
                 "drag")
                (make-world selected-cat-at-20-200
                            unselected-cat-at-300-200
                            false)
                "dragging a cat outside the canvas should move the cat image to
                 the mouse pointer location")
  (check-equal? (world-after-mouse-event 
                 (make-world unselected-cat-at-150-100
                             unselected-cat-at-300-200
                             false)
                 160 
                 110
                 "button-down")
                (make-world selected-cat-at-150-100
                            unselected-cat-at-300-200
                            false)
                "a cat should be selected iff the mouse pointer is on the image
                on button-down event")
  (check-equal? (cat-after-mouse-event unselected-cat-at-300-200 
                                       100 
                                       10
                                       "button-down")
                unselected-cat-at-300-200
                "a cat should be selected iff the mouse pointer is on the image
                on button-down event")
  (check-equal? (cat-after-mouse-event selected-cat-at-150-100 
                                       250
                                       200
                                       "drag")
                selected-cat-at-250-200
                "a cat should be dragged to the mouse pointer location only when
                it is selected")
  (check-equal? (cat-after-mouse-event unselected-cat-at-300-200
                                       100 
                                       10
                                       "drag")
                unselected-cat-at-300-200
                "a cat should be dragged to the mouse pointer location only when
                it is selected")
  (check-equal? (cat-after-mouse-event selected-cat-at-160-110 
                                       160
                                       110
                                       "button-up")
                unselected-cat-at-160-110
                "a cat should be unselected iff the mouse pointer is on the 
                image on button-up event")
  (check-equal? (cat-after-mouse-event unselected-cat-at-300-200 
                                       100 
                                       10
                                       "button-up")
                unselected-cat-at-300-200
                "a cat should be unselected iff the mouse pointer is on the 
                image on button-up event")
  (check-equal? (cat-after-mouse-event unselected-cat-at-300-200 
                                       100 
                                       10
                                       "enter")
                unselected-cat-at-300-200
                "any other mouse-event should leave the cat unchanged"))

;; in-cat? : Cat Real Real -> Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES:
;;  (in-cat? (make-cat 150 100 false "east") 160 110) => true
;;  (in-cat? (make-cat 300 200 true "north") 100 10) => false
;; STRATEGY: Structural Decomposition on Cat c
(define (in-cat? c x y)
  (and
   (<= 
    (- (cat-x-pos c) HALF-CAT-WIDTH)
    x
    (+ (cat-x-pos c) HALF-CAT-WIDTH))
   (<= 
    (- (cat-y-pos c) HALF-CAT-HEIGHT)
    y
    (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

;; TESTS:
(check-equal? (in-cat? unselected-cat-at-150-100 160 110) true 
              "in-cat? should return true iff the mouse pointer
               is on the image")
(check-equal? (in-cat? unselected-cat-at-300-200 100 10) false 
              "in-cat? should return false if the mouse pointer
               is outside the image")

;........................................................................

;; initial-world : Integer -> World
;; RETURNS: a world with two unselected cats at the given y coordinate
;; EXAMPLES:
;;  (initial-world 0) => (make-world
;;                        (make-cat 150 0 false "south")
;;                        (make-cat 300 0 false "south")
;;                        false)
;;  (initial-world 300) => (make-world
;;                          (make-cat 150 300 false "south")
;;                          (make-cat 300 300 false "south")
;;                          false)
;; STRATEGY: Function Composition
(define (initial-world y)
  (make-world
   (make-cat CAT1-X-COORD 
             y
             false 
             "south")
   (make-cat CAT2-X-COORD 
             y
             false 
             "south")
   false))

;; TESTS:
(check-equal? (initial-world 100) (make-world
                                   (make-cat 150 100 false "south")
                                   (make-cat 300 100 false "south")
                                   false)
              "initial-world is not initialising the world properly")

;........................................................................

;; cat-north? : Cat -> Boolean
;; cat-south? : Cat -> Boolean
;; cat-east?  : Cat -> Boolean
;; cat-west?  : Cat -> Boolean
;; GIVEN: a Cat c
;; RETURNS: true iff c is travelling in the specified direction.
;; EXAMPLES:
;;  (cat-north? (make-cat 23 46 true "north")) => true
;;  (cat-south? (make-cat 250 300 false "east")) => false
;; STRATEGY: Structure Decomposition
(define (cat-north? c)
  (string=? (cat-direction c) "north"))
(define (cat-south? c)
  (string=? (cat-direction c) "south"))
(define (cat-east? c)
  (string=? (cat-direction c) "east"))
(define (cat-west? c)
  (string=? (cat-direction c) "west"))

;; TESTS:
(check-equal? (cat-north? (make-cat 23 46 true "north")) true
              "cat-north? should return true iff cat is moving towards north")
(check-equal? (cat-south? (make-cat 250 300 false "east")) false
              "cat-south? should return true iff cat is moving towards south")
(check-equal? (cat-east? (make-cat 260 300 true "south")) false
              "cat-east? should return true iff cat is moving towards east")
(check-equal? (cat-west? (make-cat 203 365 false "west")) true
              "cat-west? should return true iff cat is moving towards west")
