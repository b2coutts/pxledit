#lang racket

(require "painter.rkt" "state.rkt")

(require racket/gui/base data/gvector
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

(sset! 'foo 42)

(define (drawfn)
  (text (format "var is: ~a" (sref 'foo)) 12 'black))

(define test-pic (picture
  'testpic
  1
  1
  100
  100
  (set 'foo)
  0.0
  #t
  drawfn))

(init-painter! (lambda (x) (void)) 200 200 "girffetest")
(add-pic! test-pic)
(paint!)
(printf "painted\n")
