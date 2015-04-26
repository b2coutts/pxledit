#lang racket

(require "painter.rkt" "state.rkt" "pictures.rkt")

(require racket/gui/base data/gvector
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

;; TODO: un-hardcode all of these values
(sset! 'pxwd 16)    ;; displayed width of a bmp pixel
(sset! 'xpx 20)     ;; number of columns in bmp
(sset! 'ypx 20)     ;; number of rows in bmp
(sset! 'cursor-x 0) ;; x- and y- positions of the cursor, 0-indexed
(sset! 'cursor-y 0)
(sset! 'colors (make-vector (* (sref 'xpx) (sref 'ypx)) "white"))

(for ([x (sref 'xpx)])
  (for ([y (sref 'ypx)])
    (vector-set! (srefd! 'colors) (+ (* y (sref 'xpx)) x) (color 0 (* x 10) (* y 10) 255))))

(init-painter!
  (lambda (x) (void))
  (+ (* (sref 'xpx) (sref 'pxwd)) 100)
  (max 100 (* (sref 'ypx) (sref 'pxwd)))
  "girffetest")

(add-pic! (mk-pic-background (sref 'xpx) (sref 'ypx) (sref 'pxwd)))
(add-pic! (mk-pic-pixels (sref 'xpx) (sref 'ypx) (sref 'pxwd)))
(add-pic! (mk-pic-cursor-info (sref 'xpx) (sref 'ypx) (sref 'pxwd)))

(paint!)
(printf "painted\n")
