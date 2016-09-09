;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname balls-in-box-constants) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t write repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require "balls-in-box.rkt")

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

;------------------------------------------------------------------------------

(define NEW-BALL (make-ball 200 150 0 0 false))

(define world-with-no-balls (make-world (list) 0))

(define world-with-ball-after-n-key (make-world (list NEW-BALL) 1))

(define ball1 (make-ball 302 96 150 50 false))

(define ball1-after-button-down (make-ball 302 96 290 87 false))

(define ball1-after-drag (make-ball 302 96 290 87 false))

(define ball1-after-button-up (make-ball 302 96 290 87 false))

(define ball2 (make-ball 286 92 290 87 false))
;;mx 290, 87
(define ball2-after-button-down (make-ball 286 92 290 87 true))
;;mx 150, my 50
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

(define world-with-4balls (make-world lob1 4))

(define world-with-balls-after-button-down (make-world lob1-after-button-down 4))

(define world-with-balls-after-drag (make-world lob1-after-drag 4))

(define world-with-balls-after-button-up (make-world lob1-after-button-up 4))
