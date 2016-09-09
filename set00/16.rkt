;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |16|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; bitmap : Path -> Image
; Loads image from the path
    
; above : Image ... -> Image
; Places arbitrary number of images on top of eachother

; beside : Image ... -> Image
; Places an arbitrary number of images next to eachother

; An OutlineMode is one of
;  - "outline" ... only the shapes outline is drawn
;  - "solid" ... the whole inside of the shape is filled

; rectangle : Number Number OutlineMode Color -> Image
; Creates an image of a rectangle with given width and height, drawing mode and color

; circle : Number OutlineMode Color -> Image
; Creates animage of a circle with given radius, drawing mode and color

; text : String Number Color -> Image
; Renders the given string in the given color with the given number as text size and
; returns the resulting image

; empty-scene : Number Number -> Image
; Creates an empty white rectangle with given width and height
	
; place-image : Image Number Number Image -> Image
; Places the first image into the second image with its center at the given coordinates (x/y)

(define my-image (bitmap "rocket.jpg"))

(above my-image my-image (empty-scene 20 20))

(above my-image my-image my-image (empty-scene 20 20))

(beside my-image (empty-scene 20 20) my-image)

(beside my-image (empty-scene 20 20) my-image (empty-scene 20 20) my-image)