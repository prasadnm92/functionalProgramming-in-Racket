;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require "extras.rkt")

(provide
  initial-robot
  robot-left 
  robot-right
  robot-forward
  robot-north? 
  robot-south? 
  robot-east? 
  robot-west?)

;;Radius of Robot is 15
(define north-wall (+ 0 15))
(define south-wall (- 400 15))
(define east-wall (- 200 15))
(define west-wall (+ 0 15))

;;Face is one of
;; --north
;; --south
;; --east
;; --west

(define-struct robot (x y face))
;; robot is a (make-robot Real Real String)
;;  Interpretation:
;;    x is the x-coordinate of the centre of the robot
;;    y is the y-coordinate of the centre of the robot
;;    face specifies which direction the robot is facing currently

;;DISTRUCTOR TEMPLATE:
;;robot-fn : Robot -> ??
; (define (robot-fn r)
;   (...
;    (robot-x r)
;    (robot-y r)
;    (robot-face r)))

;;initial-robot : Real Real -> Robot
;; GIVEN: a set of (x,y) coordinates
;; RETURNS: a robot with its center at the given coordinates, facing north(up).
;; EXAMPLES:
;;  (initial-robot -30 40) => (make-robot 30 40 "north")
;;  (initial-robot 250 500) => (make-robot 250 500 "north")
;;  (initial-robot 250 300) => (make-robot 250 300 "north")
;;  (initial-robot 80 450) => (make-robot 80 450 "north")
;;STRATEGY: Function Composition

(define (initial-robot x y)
  (make-robot x y "north"))

;;robot-left : Robot -> Robot
;;robot-right : Robot -> Robot
;; GIVEN: a robot
;; RETURNS: a robot like the original but turned 90 degrees, either
;;          left or right.
;; EXAMPLES:
;;   (robot-left (initial-robot 30 40)) => (make-robot 30 40 "west")
;;   (robot-right (initial-robot 80 450)) => (make-robot 80 450 "east")
;;STRATEGY: Function Composition

(define (robot-left r)
  (make-robot (robot-x r) (robot-y r) (rotate-left (robot-face r))))

(define (robot-right r)
  (make-robot (robot-x r) (robot-y r) (rotate-right (robot-face r))))

;;rotate-left : String -> String
;;rotate-right : String -> String
;; GIVEN: a string specifying the current direction of the robot
;; RETURNS: a string specifying the direction of the robot after
;;          the mentioned rotation
;; EXAMPLES:
;;   (rotate-left "south") => "east"
;;   (rotate-left "east") => "north"
;;   (rotate-right "south") => "west"
;;   (rotate-right "east") => "south"
;; STRATEGY: Cases on face (direction)

(define (rotate-left direction)
  (cond
    [(string=? direction "north") "west"]
    [(string=? direction "south") "east"]
    [(string=? direction "east") "north"]
    [(string=? direction "west") "south"]))
  
(define (rotate-right direction)
  (cond
    [(string=? direction "north") "east"]
    [(string=? direction "south") "west"]
    [(string=? direction "east") "south"]
    [(string=? direction "west") "north"]))

;;TESTS
(check-equal? (rotate-left "south") "east" 
              "robot doesn't turn left from south to east")
(check-equal? (rotate-left "east") "north" 
              "robot doesn't turn left from east to north")
(check-equal? (rotate-right "south") "west" 
              "robot doesn't turn right from south to west")
(check-equal? (rotate-right "east") "south" 
              "robot doesn't turn right from east to south")
(check-equal? (rotate-left "west") "south" 
              "robot doesn't turn left from west to south")
(check-equal? (rotate-right "west") "north" 
              "robot doesn't turn right from east to south")

;;robot-north? : Robot -> Boolean
;;robot-south? : Robot -> Boolean
;;robot-east? : Robot -> Boolean
;;robot-west? : Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: whether the robot is facing in the specified direction.
;; EXAMPLES:
;;   (robot-east? (make-robot 586 384 "east")) => true
;;   (robot-west? (make-robot 586 384 "east")) => false
;; STRATEGY: Cases on face

(define (robot-north? r)
  (string=? (robot-face r) "north"))
(define (robot-south? r)
  (string=? (robot-face r) "south"))
(define (robot-east? r)
  (string=? (robot-face r) "east"))
(define (robot-west? r)
  (string=? (robot-face r) "west"))

;;robot-forward : Robot PosInt -> Robot
;; GIVEN: a robot and a distance
;; RETURNS: a robot like the given one, but moved forward by the specified 
;;          distance. If moving forward the specified distance would cause
;;          the robot to move from being entirely inside the room to being
;;          even partially outside the room, then the robot should stop at
;;          the wall.
;; EXAMPLES:
;;  (robot-forward (make-robot 70 450 "north") 600) 
;;                           => (make-robot 70 15 "north")
;;  (robot-forward (make-robot 300 450 "north") 600) 
;;                           => (make-robot 300 -150 "north")
;;  (robot-forward (make-robot -300 450 "north") 600) 
;;                           => (make-robot -300 -150 "north")
;;  (robot-forward (make-robot -300 250 "north") 600) 
;;                           => (make-robot -300 -350 "north")
;;  (robot-forward (make-robot 300 250 "north") 600) 
;;                           => (make-robot 300 -350 "north")
;;  (robot-forward (make-robot 300 -250 "north") 100) 
;;                           => (make-robot 300 -350 "north")
;;  (robot-forward (make-robot 30 40 "north") 100) 
;;                           => (make-robot 30 15 "north")
;; STRATEGY: Structure Decomposition and Function Composition

(define (robot-forward r dist)
  (cond
    [(is-outside-room? r) (move-robot-outside-room r dist)]
    [(is-inside-room? r) (move-robot-inside-room r dist)]
    [else (move-robot r dist)]))

;;is-outside-room? : Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: true iff the robot is strictly outside all boundaries of the room,
;;          not even in the perpendicular view of any of the walls
;; EXAMPLES:
;;   (is-outside-room? (initial-robot 20 30)) => false
;;   (is-outside-room? (initial-robot 200 -15)) => true
;;   (is-outside-room? (initial-robot -100 30)) => false
;; STRATEGY: Structure Decomposition

(define (is-outside-room? r)
  (and
      (or (< (robot-x r) 15) (> (robot-x r) 185))
      (or (< (robot-y r) 15) (> (robot-y r) 385))))

;;is-inside-room? : Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: true iff the robot is completely inside all boundaries of the room
;; EXAMPLES:
;;   (is-inside-room? (initial-robot 20 30)) => true
;;   (is-inside-room? (initial-robot 200 -15)) => false
;;   (is-inside-room? (initial-robot -100 30)) => false
;; STRATEGY: Structure Decomposition

(define (is-inside-room? r)
  (and
      (and (>= (robot-x r) 15) (<= (robot-x r) 185))
      (and (>= (robot-y r) 15) (<= (robot-y r) 385))))

;;move-robot-outside-room : Robot PosInt -> Robot
;; GIVEN: a robot and a positive distance
;; RETURNS: a robot moved in the facing direction by the mentioned distance
;; EXAMPLES:
;;  (move-robot-outside-room (make-robot 300 450 "north") 600) 
;;                           => (make-robot 300 -150 "north")
;;  (move-robot-outside-room (make-robot -300 250 "north") 600) 
;;                           => (make-robot -300 -350 "north")
;; STRATEGY: Structure Decomposition and Cases

(define (move-robot-outside-room r d)
  (cond
    [(robot-north? r) (make-robot (robot-x r) (- (robot-y r) d) "north")]
    [(robot-south? r) (make-robot (robot-x r) (+ (robot-y r) d) "south")]
    [(robot-east? r) (make-robot (+ (robot-x r) d) (robot-y r) "east")]
    [(robot-west? r) (make-robot (- (robot-x r) d) (robot-y r) "west")]))

;;move-robot-inside-room : Robot PosInt -> Robot
;; GIVEN: a robot and a positive distance
;; RETURNS: a robot moved in the facing direction by the mentioned distance
;; EXAMPLES:
;;  (move-robot-inside-room (make-robot 30 45 "north") 60) 
;;                           => (make-robot 30 15 "north")
;;  (move-robot-inside-room (make-robot 100 150 "north") 10) 
;;                           => (make-robot 100 140 "north")
;; STRATEGY: Structure Decomposition and Cases

(define (move-robot-inside-room r d)
  (cond
    [(robot-north? r) (move-north (robot-x r) (robot-y r) d)]
    [(robot-south? r) (move-south (robot-x r) (robot-y r) d)]
    [(robot-east? r) (move-east (robot-x r) (robot-y r) d)]
    [(robot-west? r) (move-west (robot-x r) (robot-y r) d)]))

;;move-north : Real Real Real -> Robot
;;move-south : Real Real Real -> Robot
;;move-east : Real Real Real -> Robot
;;move-west : Real Real Real -> Robot
;; GIVEN: the X and Y coordinates of the robot's centre and the distance
;; RETURNS: a robot that has moved in the specified direction when the robot
;;          is strictly within the wall in the direction of movement
;; EXAMPLES:
;;   (move-north 30 450 1000) => (make-robot 30 15 "north")
;;   (move-south 250 30 400) => (make-robot 250 430 "south")
;; STRATEGY: Structure Decomposition

(define (move-north x y dist)
  (cond
    [(< north-wall (- y dist))
     (make-robot x (- y dist) "north")]
    [else (make-robot x north-wall "north")]))

(define (move-south x y dist)
  (cond
    [(> south-wall (+ y dist))
     (make-robot x (+ y dist) "south")]
    [else (make-robot x south-wall "south")]))

(define (move-east x y dist)
  (cond
    [(> east-wall (+ x dist))
     (make-robot (+ x dist) y "east")]
    [else (make-robot east-wall y "east")]))

(define (move-west x y dist)
  (cond
    [(< west-wall (- x dist))
     (make-robot (- x dist) y "west")]
    [else (make-robot west-wall y "west")]))

;;move-robot : Robot PosInt -> Robot
;; GIVEN: a robot and a positive distance
;; RETURNS: a robot moved in the facing direction by the mentioned distance
;; EXAMPLES:
;;  (move-robot (make-robot 30 12 "north") 100) 
;;                           => (make-robot 30 -88 "north")
;;  (move-robot (make-robot 150 286 "south") 100) 
;;                           => (make-robot 150 385 "north")
;; STRATEGY: Structure Decomposition and Cases

(define (move-robot r d)
  (cond
    [(robot-north? r) (if 
                       (< (robot-y r) 15) 
                       (move-robot-outside-room r d) 
                       (move-robot-inside-room r d))]
    [(robot-south? r) (if 
                       (> (robot-y r) 385) 
                       (move-robot-outside-room r d) 
                       (move-robot-inside-room r d))]
    [(robot-east? r) (if 
                      (< (robot-x r) 15) 
                      (move-robot-outside-room r d) 
                      (move-robot-inside-room r d))]
    [(robot-west? r) (if 
                      (> (robot-x r) 185)
                      (move-robot-outside-room r d) 
                      (move-robot-inside-room r d))]))

;;TESTS
(check-equal? (move-robot (make-robot 30 12 "north") 100) 
              (make-robot 30 -88 "north"))
(check-equal? (move-robot (make-robot 150 386 "south") 100) 
              (make-robot 150 486 "south"))
(check-equal? (move-robot (make-robot 190 200 "east") 100) 
              (make-robot 290 200 "east"))
(check-equal? (move-robot (make-robot 8 100 "west") 100) 
              (make-robot -92 100 "west"))

;;TESTS
(begin-for-test
  (check-equal? (initial-robot -30 40) (make-robot -30 40 "north")
                "robot is not getting initialised at (30,40) facing north")
  
  (check-equal? (initial-robot 250 500) (make-robot 250 500 "north")
                "robot is not getting initialised at (250,500) facing north")
  
  (check-equal? (initial-robot 250 300) (make-robot 250 300 "north")
                "robot is not getting initialised at (250,300) facing north")
  
  (check-equal? (initial-robot 80 450) (make-robot 80 450 "north")
                "robot is not getting initialised at (80,450) facing north")
  
  (check-equal? (robot-left (initial-robot 30 40)) (make-robot 30 40 "west")
                "robot at (30,40) facing north, doesn't turn left (west)")
  
  (check-equal? (robot-right (initial-robot 80 450)) (make-robot 80 450 "east")
                "robot at (80,450) facing north, doesn't turn right (east)")
  
  (check-equal? (robot-forward (make-robot 30 40 "south") 400) 
                (make-robot 30 385 "south")
                "robot should stop at south wall from inside the room")

  (check-equal? (robot-forward (make-robot 250 500 "east") 350) 
                (make-robot 600 500 "east")
                "robot should be able to travel freely outside the room")

  (check-equal? (robot-forward (make-robot 70 450 "north") 200) 
                (make-robot 70 250 "north")
                "robot should be able to enter the room from outside")

  (check-equal? (robot-forward (make-robot -100 170 "east") 350) 
                (make-robot 185 170 "east")
                "robot should able to enter room from outside
 and should not get out")

  (check-equal? (robot-forward (make-robot 250 200 "west") 50) 
                (make-robot 200 200 "west")
                "robot should be able to stop at the border, 
coming from outside the room")

  (check-equal? (robot-forward (make-robot 200 300 "east") 50) 
                (make-robot 250 300 "east")
                "robot should be able to go outside the room from
                    the border of the room")

  (check-equal? (robot-forward (make-robot 100 0 "south") 50) 
                (make-robot 100 50 "south")
                "robot should be able to enter the room from 
                    the border of the room")
  (check-equal? (robot-forward (initial-robot -50 -50) 100) 
                (make-robot -50 -150 "north")
                "robot should be able to move freely outside the room")
  (check-equal? (robot-forward (make-robot 250 30 "west") 400) 
                (make-robot 15 30 "west")
                "robot cannot exit the room once entered")
  (check-equal? (robot-forward (make-robot 185 200 "north") 400) 
                (make-robot 185 15 "north")
                "robot cannot exit from inside the room")
  (check-equal? (robot-forward (make-robot 15 300 "east") 1000)
                (make-robot 185 300 "east")
                "robot cannot exit from inside the room")
  (check-equal? (robot-forward (make-robot 250 500 "west") 350) 
                (make-robot -100 500 "west")
                "robot should be able to move freely outside the room")
  (check-equal? (robot-forward (make-robot -50 -50 "south") 50) 
                (make-robot -50 0 "south")
                "robot should be able to move freely outside the room")
  (check-equal? (robot-forward (make-robot 30 40 "east") 100)
                (make-robot 130 40 "east")
                "robot cannot exit from inside the room"))