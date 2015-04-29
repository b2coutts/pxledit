#lang racket
;; miscellaneous helper functions

(provide invert-color white black trans solidify)

(require (except-in 2htdp/image make-color make-pen))

(define/contract (invert-color col)
  (-> color? color?)
  (match-define (color red green blue alpha) col)
  (color (- 255 red) (- 255 green) (- 255 blue) alpha))

(define black (color 0 0 0 255))
(define white (color 255 255 255 255))
(define trans (color 0 0 0 0))

;; changes a color's alpha channel to 255
;; TODO: should maybe have a way of actually displaying the alpha channel, i.e. a checker background
(define/contract (solidify col)
  (-> color? color?)
  (match-define (color r g b a) col)
  (color r g b 255))
