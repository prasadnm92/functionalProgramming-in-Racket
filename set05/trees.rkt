;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Purpose statement:
;To design and implement a system for a graphical interface for trees.

;;How to run this program
;run with (run 0)

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "sets.rkt")

;;PROVIDING FUNCTIONS
(provide run
         initial-world
         world-after-mouse-event
         world-after-key-event
         world-to-roots
         node-to-center
         node-to-sons
         node-to-selected?)

;;CONSTANTS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define LENGTH-OF-SIDE-SQR 20)
(define HALF-LENGTH-OF-SIDE-SQR (/ LENGTH-OF-SIDE-SQR 2))
(define LEFT-LIMIT (+ 0 HALF-LENGTH-OF-SIDE-SQR))
(define RIGHT-LIMIT (- CANVAS-WIDTH HALF-LENGTH-OF-SIDE-SQR))
(define ZERO 0)
(define DOUBLE-CANVAS-WIDTH (* CANVAS-WIDTH 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define NEW-SON-X (* LENGTH-OF-SIDE-SQR 2))
(define NEW-SON-Y (* LENGTH-OF-SIDE-SQR 3))

(define TREE-LINE-COLOR "blue")
(define VERTICAL-LINE-COLOR "red")

(define NODE-IS-SELECTED-IMAGE (square LENGTH-OF-SIDE-SQR "solid" "green"))
(define NODE-IS-UNSELECTED-IMAGE (square LENGTH-OF-SIDE-SQR "outline" "green"))
(define NODE-IS-SELECTED-RED-IMAGE (square LENGTH-OF-SIDE-SQR "solid" "red"))




;;---------------------DATA DEFINTIONS-----------------------------------

;;STRUCTURE DEFINTION OF THE NODE
(define-struct node (x y selected? red? sub-nodes))

;;CONSTRUCTOR TEMPLATE
; A Node is a (make-node Real Real Boolean Boolean ListOf<Node>)

;;INTERPRETATION
;x - Indicates x-coordinate of the center of node.
;y - Indicates y-coordinate of the center of node.
;selected? - true iff this node is selected.
;red? - true iff this node is in a position such that, the next child, 
;       if created, will appear outside the canvas.
;sub-nodes - It is the list of child nodes.

;;TEMPLATE
;node-fn : Node -> ??
;(define (node-fn n)
;  (...
;   (node-x n)
;   (node-y n)
;   (node-selected? n)
;   (node-red? n)
;   (lon-fn (node-sub-nodes n))))

;;A World is a ListOf<Node>.
;;Interp : A world consist of list of nodes.

;;A ListOf<Node> (LON) is one of:
; -- empty -- It indicates that there is no node present.
; -- (cons Node LON) --It indicates list of node and self-referential
;                      to LON.
;;TEMPLATE
;lon-fn : LON -> ??
;(define (lon-fn lon)
;  (cond
;    [(empty? lon) ...]
;    [else (...
;           (node-fn (first lon))
;           (lon-fn (rest lon)))]))

;;A MouseEvent is one of
;'button-down' -- selects the node
;'drag' -- drags the node
;'button-up' -- unselects the node
;'other MouseEvents' --ignored

;;A KeyEvent is one of
; 't' -- Creates a new root node in the center of the top of the canvas.
; 'n' -- When a node is selected, it creates a new child node, just below
;     -- the parent node
; 'd' -- Deletes the node and its subtree, when a node is selected.
; 'u' -- Whether a node is selected or not,deletes every node,
;        whose center is in the upper half of the canvas.
; 'other KeyEvents' -- ignored.

;;Examples for testing purposes
(define initial empty)

(define NEWNODE (make-node 
                 HALF-CANVAS-WIDTH HALF-LENGTH-OF-SIDE-SQR false false empty))

(define world-after-n-initial (list NEWNODE))

(define node1 (make-node 70 170 false false empty))
(define node2 (make-node 110 170 false false empty))
(define node3 (make-node 150 170 false false empty))
(define node4 (make-node 190 170 false false empty))
(define node5 (make-node 150 110 false false (list node1 node2 node3)))
(define node6 (make-node 190 110 false false (list node4)))
(define node7 (make-node 190 50 false false (list node5 node6)))

(define node8 (make-node 30 280 true false empty))
(define node9 (make-node 70 280 false false empty))
(define node10 (make-node 70 220 false false (list node8 node9)))

(define node11 (make-node 380 200 false false empty))
(define node12 (make-node 300 180 false false empty))
(define node13 (make-node 320 140 false false (list node11 node12)))
(define node14 (make-node 220 140 false false empty))
(define node15 (make-node 240 100 false false (list node13 node14)))
(define node16 (make-node 30 280 true true 
                          (list (make-node 30 340 false false empty))))
(define node17 (make-node 5 170 false false empty))
(define node18 (make-node 30 300 true false empty))

(define node23 (make-node 30 340 false false empty))
(define node19 (make-node 30 280 false false (list node23)))
(define node20 (make-node 70 220 true true (list node19 node9)))

(define node21 (make-node 320 140 true false (list node11 node12)))
(define node22 (make-node 240 100 false false (list node21 node14)))

(define tree1 node7)
(define tree2 node10)
(define tree3 node15)
(define tree4 node11)
(define tree5 node8)
(define tree6 node18)
(define tree7 node20)
(define tree8 node22)

(define world-with-1-tree (list tree3))
(define world-with-2-trees (list tree1 tree2))
(define world-with-tree1 (list tree4))
(define world-with-tree2 (list tree5))
(define world-with-tree3 (list tree6))
(define world-with-tree4 (list node16))

(define image-of-tree2 (scene+line
                        (scene+line
                         (place-image 
                          NODE-IS-SELECTED-IMAGE 30 280
                          (scene+line
                           (place-image
                            NODE-IS-UNSELECTED-IMAGE 70 280
                            (place-image
                             NODE-IS-UNSELECTED-IMAGE 70 220
                             EMPTY-CANVAS))
                           70 220 70 280 "blue"))
                         70 220 30 280 "blue")
                        20 CANVAS-HEIGHT 20 ZERO "red"))

(define image-of-tree7 (scene+line
                        (place-image
                         NODE-IS-UNSELECTED-IMAGE 30 340
                         (scene+line
                          (place-image
                           NODE-IS-UNSELECTED-IMAGE 30 280
                           (scene+line
                            (place-image
                             NODE-IS-UNSELECTED-IMAGE 70 280
                             (scene+line
                              (place-image 
                               NODE-IS-SELECTED-RED-IMAGE 70 220 
                               EMPTY-CANVAS)
                              -10 CANVAS-HEIGHT -10 ZERO "red"))
                            70 220 70 280 "blue"))
                          70 220 30 280 "blue"))
                        30 280 30 340 "blue"))

;;-----------------------------------------------------------------------

;run : Any -> World
;GIVEN : Any Value
;EFFECT : runs a copy of an initial world
;RETURNS : the final state of the world . The given value is ignored.
;EXAMPLE : (run 5) => creates and runs a copy of an initial world.It returns
;          the final state of the world. The given input value is ignored.

(define(run any)
  (big-bang (initial-world any)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)))

;;--------------------------FUNCTION DEFINTIONS--------------------------

;initial-world : Any -> World
;GIVEN: any value
;RETURNS: an initial world. The given value is ignored.
;EXAMPLE: 
;   (initial-world 5) => (make-world empty)
;STRATEGY: Function Composition

(define (initial-world any)
  empty)

;;TESTS:
(begin-for-test
  (check-equal? (initial-world 5)
                empty
                "Failed to display the intial world"))

;;-----------------------------------------------------------------------

;world-to-scene : World -> Scene
;GIVEN: a World.
;RETURNS: a scene with the trees of the given world drawn on it.
;EXAMPLES:
;   (world-to-scene (list tree2)) => image-of-tree2
;   (world-to-scene (list tree7)) => image-of-tree7
;STRATEGY: HOFC
(define (world-to-scene w)
  (foldr
   (;Node Scene -> Scene
    ;GIVEN: a node and a scene with set of nodes seen so far placed on it.
    ;RETURNS: a scene with given node placed on it.
    lambda (root scene-rest-world)
     (tree-to-scene root scene-rest-world))
   EMPTY-CANVAS
   w))

;;Tests for world-to-scene
(begin-for-test
  (check-equal? (world-to-scene (list tree2)) image-of-tree2
                "The image for tree2 is not being displayed properly")
  (check-equal? (world-to-scene (list tree7)) image-of-tree7
                "The image for tree7 is not being displayed properly"))


;tree-to-scene : Node Scene -> Scene
;GIVEN: a node and a scene with a previous set of nodes placed in it.
;RETURNS: a scene with the given node placed in it.
;EXAMPLE:
;   (tree-to-scene tree2 EMPTY-CANVAS) => image-of-tree2
;STRATEGY: Structural Decomposition on root : Node
(define (tree-to-scene root scene-rest-world)
  (foldr
   (;Node Scene -> Scene
    ;GIVEN: a node and a scene with the previous nodes placed in it
    ;RETURNS: a scene, with the given node placed in it
    lambda (n scene-rest-tree)
     (children-to-scene n (node-x root) (node-y root) scene-rest-tree))
   (root-to-scene (left-most-child-x root) (node-x root) (node-y root)
                  (node-red? root) (node-selected? root) scene-rest-world)
   (node-sub-nodes root)))

;root-to-scene : Real Real Real Boolean Boolean Scene -> Scene
;GIVEN: the x-coordinate of the left most child of a node who's centered at the
;       given x and y coordinates, boolean values that describe if the node is
;       red and/or selected, and a scene with a set of previous nodes placed
;RETURNS: a scene with the node placed in it
;EXAMPLE:
;   (root-to-scene 230 200 50 false true EMPTY-CANVAS) => 
;                  (place-image NODE-IS-UNSELECTED-IMAGE 200 50 EMPTY-CANVAS)
;STRATEGY: Function Composition
(define (root-to-scene next-child-x x y red? selected? scene-rest-tree)
  (if selected?
      (scene+line (draw-root x y red? selected? scene-rest-tree)
                  (- (- next-child-x NEW-SON-X) HALF-LENGTH-OF-SIDE-SQR)
                  CANVAS-HEIGHT
                  (- (- next-child-x NEW-SON-X) HALF-LENGTH-OF-SIDE-SQR) 
                  ZERO 
                  VERTICAL-LINE-COLOR)
      (draw-root x y red? selected? scene-rest-tree)))

;draw-root : Real Real Boolean Boolean Scene -> Scene
;GIVEN: the x and y coordinates of a node, the boolean values that describe
;       if the node is red and/or selected, and a scene with previous nodes
;       placed in it.
;RETURNS: a scene with this node placed in it.
;EXAMPLE:
;   (draw-root 250 200 false true EMPTY-CANVAS) =>
;   (place-image NODE-IS-SELECTED-IMAGE 250 200 EMPTY-CANVAS)
;STRATEGY: Function Composition
(define (draw-root x y red? selected? scene-rest-tree)
  (place-image (node-image red? selected?) x y scene-rest-tree))

;node-image : Boolean Boolean -> Image
;GIVEN: boolean values that describe if the node is red and/or selected.
;RETURNS: an image representing the node, a green solid node if its seleceted
;        a red solid node if its "red", a green outlined node if none of these.
;EXAMPLE:
;   (node-image true true) => NODE-IS-SELECTED-RED-IMAGE
;STRATEGY: Function Composition
(define (node-image red? selected?)
  (if red? NODE-IS-SELECTED-RED-IMAGE
      (if selected? NODE-IS-SELECTED-IMAGE NODE-IS-UNSELECTED-IMAGE)))

;children-to-scene : Node Real Real Scene -> Scene
;GIVEN: a node, the x and y coordnates of given node's parent, and a scene with
;       previous nodes placed in it.
;RETURNS: a scene with the given node placed in it.
;EXAMPLE:
;   (children-to-scene node9 70 220 
;                    (place-image NODE-IS-UNSELECTED-IMAGE 70 220 EMPTY-CANVAS))
;  => (scene+line (place-image NODE-IS-UNSELECTED-IMAGE 70 280 
;                   (place-image NODE-IS-UNSELECTED-IMAGE 70 220 EMPTY-CANVAS))
;                     70 220 70 280 "blue")
;STRATEGY: Structural Decomposition on n : Node
(define (children-to-scene n parent-x parent-y scene-rest-tree)
  (foldr
   (;Node Scene -> Scene
    ;GIVEN: a node and a scene with previous nodes placed in it
    ;RETURNS: a scene with the given node placed in it
    lambda (child scene)
     (children-to-scene child (node-x n) (node-y n) scene))
   (child-to-scene (left-most-child-x n) (node-x n) (node-y n) (node-red? n)
                   (node-selected? n) parent-x parent-y scene-rest-tree)
   (node-sub-nodes n)))

;child-to-scene : Real Real Real Boolean Boolean Real Real Scene -> Scene
;GIVEN: the x-coordinate of the left most child of a node who's centered at the
;       given x and y coordinates, boolean values that describe if the node is
;       red and/or selected, the x and y coordinates of the parent of the node
;       being placed now, and a scene with a set of previous nodes placed.
;RETURNS: a scene with the node placed in it.
;EXAMPLE:
;   (child-to-scene  140 100 300 false false 100 240 
;                  (place-image NODE-IS-UNSELECTED-IMAGE 100 240 EMPTY-CANVAS))
;    =>(scene+line (place-image NODE-IS-UNSELECTED-IMAGE 100 300 
;                              (place-image NODE-IS-UNSELECTED-IMAGE 100 240
;                                           EMPTY-CANVAS))
;                   100 240 100 300 "blue")
;STRATEGY: Function Composition
(define (child-to-scene next-child-x x y red? selected? px py scene)
  (scene+line (root-to-scene next-child-x  x y red? selected? scene)
              px py x y TREE-LINE-COLOR))

;;-----------------------------------------------------------------------

;world-after-mouse-event : World Integer Integer MouseEvent -> World
;GIVEN: a World, location of the mouse pointer, and a MouseEvent.
;RETURNS: the state of the world as it should be following the given 
;         mouse event at that location.
;EXAMPLES: (world-after-mouse-event world-with-1-tree 100 110 "button-down") =>
;          (list node15)
;          (world-after-mouse-event world-with-1-tree 100 110 "button-up") =>
;          (list node15)
;STRATEGY: Cases on mev : MouseEvent

(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up") (world-after-button-up w mx my)]
    [else w]))

;;Test Cases for MouseEvent

(begin-for-test
  (check-equal? (world-after-mouse-event world-with-1-tree 
                                         100 110 "button-down")
                (list node15)
                "Failed to not select any node in the world 
                 after button down event where no node was present")
  
  (check-equal? (world-after-mouse-event world-with-1-tree 
                                         100 110 "button-up")
                (list node15)
                "Failed to unselect all nodes after the button up event")
  
  (check-equal? (world-after-mouse-event world-with-tree4 
                                         100 110 "drag")
                (list
                 (make-node
                  100
                  110
                  true
                  true
                  (list (make-node 100 170 false false empty))))
                "Failed to move the selected nodes to the new mouse 
                 location after drag event")
  
  (check-equal? (world-after-mouse-event world-with-tree1
                                         100 110 "drag")
                (list node11)
                "Failed to not drag when no node was selected after drag event")
  
  (check-equal? (world-after-mouse-event world-with-tree1 360 190 "leave")
                (list node11)
                "Failed to ignore the mouse event 'leave'")
  
  (check-equal? (node-after-button-down node17 5 170)
                (make-node 5 170 true true empty)
                "Failed to select a node after button down 
                 event inside the node"))

;;-----------------------------------------------------------------------

;world-after-button-down : World Integer Integer -> World
;GIVEN: a node and the x and y coordinates of the mouse pointer.
;RETURNS: a node, with its selected? and red? values changed to reflect the
;         button-down event.
;EXAMPLE:
;   (world-after-button-down world-with-1-tree 100 110) =>
;   (list node15)
;STRATEGY: HOFC
(define (world-after-button-down w mx my)
  (map
   (;Node -> Node
    ;GIVEN: a node.
    ;RETURNS: a node, just like the given, after a button-down event has been
    ;         applied on it.
    lambda (node)
     (node-after-button-down node mx my))
   w))

;node-after-button-down : Node Integer Integer -> Node
;GIVEN: a node and the x and y coordinates of the mouse pointer.
;RETURNS: a node, with its selected? and red? values changed to reflect the
;         button-down event.
;EXAMPLE:
;   (node-after-button-down node15 100 110) => node15  
;STRATEGY: Structural Decomposition on root : Node
(define (node-after-button-down root mx my)
  (make-node (node-x root) (node-y root)
             (button-down-in-node? root mx my)
             (and (button-down-in-node? root mx my)
                  (next-child-off-canvas? root))
             (world-after-button-down (node-sub-nodes root) mx my)))

;next-child-off-canvas? : Node -> Boolean
;GIVEN: a node.
;RETURNS: true iff the next child of the given node, if created, appears
;         outside the canvas.
;EXAMPLES:
;   (next-child-off-canvas? node19) => true
;   (next-child-off-canvas? node22) => false
;STRATEGY: Function Composition
(define (next-child-off-canvas? n)
  (<= (- (left-most-child-x n) NEW-SON-X) LEFT-LIMIT))

;mouse-event-in-node? : Node Integer Integer -> Boolean
;GIVEN: a node and the x and y coordinates of the mouse pointer.
;RETURNS: true iff the mouse pointer is inside the boundary of the image of the
;         given node.
;EXAMPLES:
;   (button-down-in-node? node20 75 230) => true
;   (button-down-in-node? node21 75 230) => false
;STRATEGY: Structural Decomposition on root : Node
(define (button-down-in-node? root mx my)
  (and (<= (- (node-x root) HALF-LENGTH-OF-SIDE-SQR)
           mx
           (+ (node-x root) HALF-LENGTH-OF-SIDE-SQR))
       (<= (- (node-y root) HALF-LENGTH-OF-SIDE-SQR)
           my
           (+ (node-y root) HALF-LENGTH-OF-SIDE-SQR))))

;;-------------------------------------------------------------------------

;world-after-drag : World Integer Integer -> World
;GIVEN: a world and the x and y coordinates of the mouse pointer.
;RETURNS: a world, as it should be after a button-down event
;EXAMPLE:
;   (world-after-drag world-with-tree1 100 110) =>
;    (list node11)
;STRATEGY: HOFC
(define (world-after-drag w mx my)
  (map
   (;Node -> Node
    ;GIVEN: a node.
    ;RETURNS: a node, moved to the new mouse position if the node was selected.
    lambda (root)
     (node-after-drag root mx my))
   w))

;node-after-drag : Node Integer Integer -> Node
;GIVEN: a node and x and y coordinates of the mouse.
;RETURNS: a node, moved to the new mouse position if the node was selected.
;EXAMPLE:
;   (node-after-drag node11 100 110) => node11
;STRATEGY: Structural Decomposition on n : Node
(define (node-after-drag n mx my)
  (if (node-selected? n)
      (make-node mx my true (next-child-off-canvas? n) (children-after-drag 
                                                        (node-sub-nodes n) 
                                                        (node-x n) 
                                                        (node-y n)
                                                        mx
                                                        my))
      (make-node (node-x n) (node-y n) (node-selected? n) (node-red? n)
                 (world-after-drag (node-sub-nodes n) mx my))))

;children-after-drag : LON Real Real Integer Integer -> LON
;GIVEN: a list of nodes, x and y coordinates of the parent node 
;       and x and y coordinates of the mouse.
;RETURNS: a list of nodes with each node changed to reflect the 
;         affect of a drag event.
;EXAMPLE:
;   (children-after-drag (list node11 node12) 100 110) =>
;    (list node11 node12)
;STRATEGY: HOFC
(define (children-after-drag lon px py mx my)
  (map
   (;Node -> Node
    ;GIVEN: a node.
    ;RETURNS: a node moved to the new mouse position, if it was selected.
    lambda (child)
     (child-after-drag child px py mx my))
   lon))

;child-after-drag : Node Real Real Integer Integer -> Node
;GIVEN: a node, x and y coordinates of the parent node 
;       and x and y position of the mouse.
;RETURNS: a node moved to the new mouse position if it was selected.
;EXAMPLE:
;   (child-after-drag (make-node 30 340 false false empty) 30 280 100 110) =>
;    (make-node 100 170 false false empty)
;STRATEGY: Structural Decomposition on child : Node
(define (child-after-drag child px py mx my)
  (make-node (+ mx (- (node-x child) px))
             (+ my (- (node-y child) py))
             (node-selected? child)
             (node-red? child)
             (children-after-drag (node-sub-nodes child) px py mx my)))


;;-------------------------------------------------------------------------

;world-after-button-up : World Integer Integer -> World
;GIVEN: a world and the x and y coordinates of the mouse pointer.
;RETURNS: a world, as it should be after a button-up event
;EXAMPLE:
;   (world-after-button-up world-with-1-tree 100 110) => (list node15)
;STRATEGY: HOFC
(define (world-after-button-up w mx my)
  (map
   (;Node -> Node
    ;GIVEN: a node.
    ;RETURNS: a node, which is made unselected.
    lambda (node)
     (node-after-button-up node mx my))
   w))

;node-after-button-up : Node Integer Integer -> Node
;GIVEN: a node and x and y coordinates of the mouse.
;RETURNS: a node, which is made unselected.
;EXAMPLE:
; (node-after-button-up node18 30 300) => (make-node 30 300 false false empty)
;STRATEGY: Structural Decomposition on root : Node
(define (node-after-button-up root mx my)
  (make-node
   (node-x root)
   (node-y root)
   false
   false
   (world-after-button-up (node-sub-nodes root) mx my)))

;left-most-child-x : Node -> Real
;GIVEN: a node.
;RETURNS: the position of the next child, if created, of the given node.
;EXAMPLE:
;   (left-most-child-x node20) => 30
;STRATEGY: Structural Decomposition on n : Node
(define (left-most-child-x n)
  (foldr
   (;Node Real -> Real
    ;GIVEN: a node and a value describing the position 
    ;       of the left most child of the given node seen so far.
    ;RETURNS: the position of the left most child of the given node.
    lambda (child min-x)
     (min min-x (node-x child)))
   (default-case-for-min-x n)
   (node-sub-nodes n)))

;default-case-for-min-x : Node -> Real
;GIVEN: a node.
;RETURNS: the x coordinate of the given node if it has no children, else the
;         maximum possible x position of a child (i.e., double canvas width).
;EXAMPLES:
;   (left-most-child-x node20) => DOUBLE-CANVAS-WIDTH
;   (left-most-child-x node23) => 30
;STRATEGY: Structural Decomposition on n : Node
(define (default-case-for-min-x n)
  (if (empty? (node-sub-nodes n)) 
      (+ (node-x n) NEW-SON-X)
      DOUBLE-CANVAS-WIDTH))
;;-----------------------------------------------------------------------

;world-after-key-event : World KeyEvent -> World
;GIVEN: a World 'w' and a key event 'kev'
;RETURNS: the state of the world as it should be following the given 
;         key event
;EXAMPLES:
;  (world-after-key-event world-with-1-tree "t") => (list NEWNODE node15)
;  (world-after-key-event world-with-tree2 "n") => (list node16)
;STRATEGY: Cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev "t") (world-after-t-key w)]
    [(key=? kev "n") (world-after-n-key w)]
    [(key=? kev "d") (world-after-d-key w)]
    [(key=? kev "u") (world-after-u-key w)]
    [else w]))

;;Test Cases for KeyEvent :
(begin-for-test
  (check-equal? (world-after-key-event world-with-1-tree "t")
                (list NEWNODE node15)
                "Failed to create new tree on 't' event")
  
  (check-equal? (world-after-key-event world-with-tree2 "n")
                (list node16)
                "Failed to create new node at the selected node on 'n' event")
  
  (check-equal? (world-after-key-event world-with-1-tree "n")
                (list node15)
                "Failed to not create a new node on 'n' event when no node
                 was selected")
  
  (check-equal? (world-after-key-event world-with-tree1 "d")
                (list node11)
                "Failed to not delete any node 'd' event when no node was
                 selected")
  
  (check-equal? (world-after-key-event world-with-tree1 "u")
                (list (make-node 380 200 false false empty))
                "Failed to delete all nodes in the upper half on 'u' event")
  
  (check-equal? (world-after-key-event world-with-tree1 "up")
                (list node11)
                "Failed to ignore the KeyEvent 'up'")
  
  (check-equal? (node-add-first-child node17)
                node17
                "Failed to add first child when node-x value was less
                than half length square")
  
  (check-equal? (node-add-child node6)
                (make-node
                 190
                 110
                 false
                 false
                 (list
                  (make-node 150 170 false false empty)
                  (make-node 190 170 false false empty)))
                "Failed to add child to the mentioned node")
  
  (check-equal? (check-and-add-more-child node17 (list node17))
                (list (make-node 5 170 false false empty))
                "Failed to return same lon as no more left child
                 addition was possible")
  
  (check-equal? (world-after-d-key world-with-tree3)
                empty
                "Failed to delete the tree on 'd' 
                event when root was selected")
  
  (check-equal? (world-after-u-key world-with-tree3)
                (list (make-node 30 300 true false empty))
                "Failed to not delete any nodes on 'u' event when no node
                 was selected"))

;;-------------------------------------------------------------------------

;world-after-t-key : World -> World
;GIVEN: a World 'w'
;RETURNS: a world with a new node created at the centre of the canvas and
;         tangent to the top boundary.
;EXAMPLE: 
;   (world-after-t-key world-with-tree2) => (list NEWNODE NODE8)
;STRATEGY: Function Composition
(define (world-after-t-key w)
  (cons (make-node 
         HALF-CANVAS-WIDTH HALF-LENGTH-OF-SIDE-SQR 
         false false empty) w))

;;-------------------------------------------------------------------------

;world-after-n-key : World -> World
;GIVEN: a World 'w'
;RETURNS: the state of the world after the 'n' keyevent.
;EXAMPLE: 
;  (world-after-n-key world-with-tree1) => (list node11)
;STRATEGY: HOFC
(define (world-after-n-key w)
  (map node-after-n-key w))

;node-after-n-key : Node -> Node
;GIVEN: a Node 'n'.
;RETURNS: a node with a new child added iff it was selected.
;EXAMPLE: 
;   (node-after-n-key node11) => node11
;STRATEGY: Structural Decomposition on n : Node
(define (node-after-n-key n)
  (if (node-selected? n)
      (node-add-child n)
      (make-node
       (node-x n)
       (node-y n)
       (node-selected? n)
       (node-red? n)
       (world-after-n-key (node-sub-nodes n)))))

;node-add-child : Node -> Node
;GIVEN: a Node 'n'.
;RETURNS: a Node after adding a child node.
;EXAMPLE: 
;  (node-add-child node11) => 
;                  (make-node 380 200 false false
;                             (list (make-node 380 260 false false empty)))
;STRATEGY: Structural Decomposition on n : Node
(define (node-add-child n)
  (if (empty? (node-sub-nodes n))
      (node-add-first-child n)
      (make-node
       (node-x n)
       (node-y n)
       (node-selected? n)
       (predict-next-child-off-canvas? n)
       (world-after-n-key (check-and-add-more-child n (node-sub-nodes n))))))

;node-add-first-child : Node -> Node
;GIVEN: a Node 'n'.
;RETURNS:  a node with the first child being added to it.
;EXAMPLES: 
;  (node-add-first-child node11) => 
;        (make-node 380 200 false false
;                   (list (make-node 380 260 false false empty)))
;STRATEGY: Structural Decomposition on n : Node
(define (node-add-first-child n)
  (if (<= (node-x n) HALF-LENGTH-OF-SIDE-SQR)
      n
      (make-node (node-x n) (node-y n) (node-selected? n) 
                 (predict-next-child-off-canvas? n)
                 (list 
                  (make-node (node-x n) (+ (node-y n) NEW-SON-Y) false false
                             empty)))))

;check-and-add-more-child : Node LON -> LON
;GIVEN: a Node 'n' and a list on nodes 'LON'.
;RETURNS: a list of nodes of the given node's children, with a new child added
;         iff it does not appear outside the canvas.
;EXAMPLES: 
; (check-and-add-more-child node17 tree6) => 
;                                     (make-node 30 300 true false empty)
;STRATEGY:Structural Decomposition on n : Node
(define (check-and-add-more-child n lon)
  (if (next-child-off-canvas? n)
      lon
      (cons (make-node (- (left-most-child-x n) NEW-SON-X) 
                       (+ (node-y n) NEW-SON-Y) false false empty) lon)))

;predict-next-child-off-canvas? : Node -> Boolean
;GIVEN: a node.
;RETURNS: true iff a child, when created, appears outside the canvas after
;         another child was created.
;EXAMPLES:
;   (predict-next-child-off-canvas? node8) => true
;   (predict-next-child-off-canvas? node1) => false
;STRATEGY: Function Composition
(define (predict-next-child-off-canvas? n)
  (<= (- (left-most-child-x n) (* NEW-SON-X 2)) 
      LEFT-LIMIT))

;;-------------------------------------------------------------------------

;world-after-d-key : World -> World
;GIVEN: a World 'w'.
;RETURNS: the state of the world after the 'd' keyevent.
;EXAMPLES: 
;   (world-after-d-key world-with-tree3) => empty
;STRATEGY: Structural Decomposition on root : Node

(define (world-after-d-key w)
  (map
   node-after-d-key
   (filter
    (;Node -> Boolean
     ;GIVEN: a node.
     ;RETURNS: true iff the node is selected.
     lambda (root)
      (not (node-selected? root)))
    w)))

;node-after-d-key : Node -> Node
;GIVEN: a Node 'n'.
;RETURNS: a node, just like the given, with its children checked for the 
;         effect of a d-key event.
;EXAMPLES: 
;   (node-after-d-key node11) => (make-node 380 200 false false empty)
;STRATEGY: Structural Decomposititon on n : Node
(define (node-after-d-key n)
  (make-node (node-x n) (node-y n) (node-selected? n) (node-red? n)
             (world-after-d-key (node-sub-nodes n))))

;;-------------------------------------------------------------------------

;world-after-u-key : World -> World
;GIVEN: a World 'w'
;RETURNS: the state of the world after the 'u' keyevent.
;EXAMPLES: 
;   (world-after-u-key world-with-tree1) => empty
;STRATEGY: Structural Decomposition on root : Node
(define (world-after-u-key w)
  (map
   node-after-u-key
   (filter
    (;Node -> Boolean
     ;GIVEN: a node.
     ;RETURNS: true iff node's y coordinate is less than canvas' half height
     lambda (root)
      (not (< (node-y root) HALF-CANVAS-HEIGHT)))
    w)))

;node-after-u-key : Node -> Node
;GIVEN: A node 'n'
;RETURNS: a node, just like the given, with its children checked for the 
;         effect of a u-key event.
;EXAMPLES:
;   (node-after-u-key world-with-tree1) => empty
;STRATEGY: Structural Decomposition on n : Node
(define (node-after-u-key n)
  (make-node (node-x n) (node-y n) (node-selected? n) (node-red? n)
             (world-after-u-key (node-sub-nodes n))))

;;-----------------------------------------------------------------------

;world-to-roots : World -> ListOf<Node>
;GIVEN: a World.
;RETURNS: a list of all the root nodes in the given world.
;EXAMPLE: 
;    (world-to-roots world-with-tree1) => (list node11)
;STRATEGY: Function Composition

(define (world-to-roots w)
  w)

;;-----------------------------------------------------------------------

;node-to-center : Node -> Posn
;GIVEN: a node.
;RETURNS: the center of the given node as it is to be displayed on the scene.
;EXAMPLES:
;   (node-to-center node17) => (make-posn 5 170)
;STRATEGY: Structural Decomposition on n : Node

(define (node-to-center n)
  (make-posn (node-x n) (node-y n)))

;;-----------------------------------------------------------------------

;node-to-sons : Node -> ListOf<Node>
;GIVEN: a node.
;RETURNS: the list of children (sub-nodes) in the given node.
;EXAMPLES: 
;    (node-to-sons node17) => empty
;STRATEGY: Structural Decomposition on n : Node

(define (node-to-sons n)
  (node-sub-nodes n))

;;-----------------------------------------------------------------------

;node-to-selected? : Node -> Boolean
;GIVEN: a node.
;RETURNS: true iff the given node is selected.
;EXAMPLES: 
;   (node-to-selected? node16) => true
;STRATEGY: Structural Decomposition on n : Node

(define (node-to-selected? n)
  (node-selected? n))

;;-----------------------------------------------------------------------

;;Test Cases

(begin-for-test
  (check-equal? (node-to-center node17)
                (make-posn 5 170)
                "Failed to provide x and y position for given node")
  
  (check-equal? (node-to-sons node17)
                empty
                "Failed to display sons for the given node")
  
  (check-equal? (node-to-selected? node16)
                true
                "Failed to display true/false when the node is selected")
  
  (check-equal? (world-to-roots world-with-tree1)
                (list node11)
                "Failed to display the tree for given world"))

;;-------------------------------------------------------------------------
