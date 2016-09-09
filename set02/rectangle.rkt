;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;;run with (run 0)
(provide run 
         initial-world
         world-x
         world-y
         world-selected?
         world-after-mouse-event)

;; MAIN FUNCTION.
;; run : Any -> World
;; GIVEN: any value
;; EFFECT: runs the simulation, starting with a rectangle at the centre of the
;;         canvas
;; RETURNS: the final state of the world
(define (run num)
  (big-bang (make-world 200 150 0 0 false)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)))

;; CONSTANTS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define RECT (rectangle 100 60 "solid" "green"))
(define RECT-SELECTED (rectangle 100 60 "outline" "green"))
(define HALF-RECT-WIDTH  (/ (image-width  RECT) 2))
(define HALF-RECT-HEIGHT (/ (image-height RECT) 2))
(define CIRCLE (circle 5 "solid" "red"))

;;; DATA DEFINITIONS
(define-struct world (x y mouse-x mouse-y selected?))
;; A World is a (make-world Real Real Boolean)
;; Interpretation: 
;; x, y give the position of the rectangle.
;; mouse-x, mouse-y give the position of the mouse pointer
;; selected? describes whether or not the rectangle is selected.
;; TEMPLATE:
;; world-fn : World -> ??
;(define (world-fn w)
; (... (world-x w) (world-y w) 
;      (world-mouse-x w) (world-mouse-y w)
;      (world-selected? w)))

;; examples of worlds, for testing
(define world-at-start (make-world 200 150 0 0 false))
(define selected-start-world-with-mouse-at-230-160 
  (make-world 200 150 230 160 true))
(define selected-world-before-drag
  (make-world 200 150 225 165 true))
(define selected-world-after-drag
  (make-world 60 40 85 55 true))
(define selected-world-with-mouse-at-180-90 
  (make-world 150 70 180 90 true))
(define unselected-world-with-mouse-at-180-90 
  (make-world 150 70 180 90 false))
(define world-with-mouse-ouside-rectangle
  (make-world 150 70 250 150 false))
(define some-world
  (make-world 230 200 150 70 false))


;; initial-world : Any -> World
;; GIVEN: takes any value
;; RETURNS: a world with a solid green rectangle placed at those coordinates
;; EXAMPLES:
;;   (initial-world 20000) => (make-world 200 150 0 0 false)
;;   (initial-world 100) => (make-world 200 150 0 0 false)
;; STRATEGY: Function Composition

(define (initial-world any)
  (make-world 200 150 0 0 false))

;; world->scene : World -> Scene
;; GIVEN: a World
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world->scene (make-world 200 150 0 0 false))
;;          = (place-image RECT 200 150 0 0 EMPTY-CANVAS)
;; STRATEGY: Structural Decomposition on World w

(define (world->scene w)
  (if (world-selected? w)
      (place-image RECT-SELECTED
                   (world-x w)
                   (world-y w)
                   (place-image CIRCLE
                                (world-mouse-x w)
                                (world-mouse-y w)
                                EMPTY-CANVAS))
      (place-image RECT
                   (world-x w)
                   (world-y w)
                   EMPTY-CANVAS)))

;; world-after-mouse-event : World Number Number MouseEvent -> World
;; GIVEN: a world, x and y coordinates of the mouse and its event
;; RETURNS: the world that should follow the given mouse event
;; EXAMPLES: 
;;
;;
;; STRATEGY: Cases on MouseEvent mev

(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

;; world-after-button-down : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a button-down at the given location.
;;          if the button-down is inside the rectangle,
;;                return an outline of the rectangle and
;;                a red circle at the mouse pointer
;; EXAMPLES:
;;
;;
;; STRATEGY: Structural Decomposition on World w

(define (world-after-button-down w mx my)
  (if (in-rect? w mx my)
      (make-world (world-x w) (world-y w) mx my true)
      w))


;; world-after-drag : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a drag at the given location.
;;          if the world is selected, then return a world just like the given
;;          one, except that it is now centered on the mouse position.
;; EXAMPLES:
;;
;;
;; STRATEGY: Structural Decomposition on World w

(define (world-after-drag w mx my)
  (if (world-selected? w)
      (make-world (+ mx (- (world-x w) (world-mouse-x w)))
                  (+ my (- (world-y w) (world-mouse-y w)))
                  mx
                  my
                  true)
      w))

;; world-after-button-up : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: the world following a button-up at the given location.
;;          return a solid unselected rectangle at the given location
;; EXAMPLES:
;;
;;
;; STRATEGY: Structural Decomposition on World w

(define (world-after-button-up w mx my)
  (make-world (world-x w) (world-y w) mx my false))
 
;; in-rect? : World Number Number -> World
;; GIVEN: a world and coordinates of the mouse pointer
;; RETURNS: true iff the given coordinate is inside the bounding box of
;;          the rectangle.
;; EXAMPLES: 
;;
;;
;; STRATEGY: Structural Decomposition on World w

(define (in-rect? w x y)
  (and
    (<= 
      (- (world-x w) HALF-RECT-WIDTH)
      x
      (+ (world-x w) HALF-RECT-WIDTH))
    (<= 
      (- (world-y w) HALF-RECT-HEIGHT)
      y
      (+ (world-y w) HALF-RECT-HEIGHT))))

;; TESTS:

(begin-for-test
  (check-equal? (initial-world 2)
                (make-world 200 150 0 0 false)
                "the world is not being initialised properly")
  (check-equal? (world->scene world-at-start)
                (place-image RECT 200 150 EMPTY-CANVAS)
                "world is not getting initialised properly")
  (check-equal? (world->scene selected-start-world-with-mouse-at-230-160)
                (place-image RECT-SELECTED 200 150 
                             (place-image CIRCLE 230 160 EMPTY-CANVAS))
                "world with selected rectangle is not proper")
  (check-equal? (world-after-mouse-event unselected-world-with-mouse-at-180-90
                                         180
                                         90
                                         "button-down")
                selected-world-with-mouse-at-180-90
                "mouse-button-down event is not behaving proprly")
  (check-equal? (world-after-button-down world-with-mouse-ouside-rectangle
                                         250 
                                         150)
                world-with-mouse-ouside-rectangle
                "mouse-button-down event should return same world when 
               mouse is outside the rectangle")
  (check-equal? (world-after-mouse-event selected-world-before-drag 
                                         85 
                                         55 
                                         "drag")
                selected-world-after-drag
          "dragging when pointer is inside rectangle should move the recangle")
  (check-equal? (world-after-drag world-with-mouse-ouside-rectangle 230 150)
                world-with-mouse-ouside-rectangle
          "dragging pointer outside rectangle should not move the rectangle")
  (check-equal? (world-after-mouse-event selected-world-with-mouse-at-180-90
                                       180
                                       90
                                       "button-up")
                unselected-world-with-mouse-at-180-90
                "buttom-up event should deselect the rectangle")
  (check-equal? (world-after-mouse-event some-world
                                         150
                                         70
                                         "leave")
                some-world
                "world should not change on an unrecognised mouse event"))