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
(sset! 'cursor-visible? #t)

(for ([x (sref 'xpx)])
  (for ([y (sref 'ypx)])
    (vector-set! (srefd! 'colors) (+ (* y (sref 'xpx)) x) (color 0 (* x 10) (* y 10) 255))))

;; helper function, which moves the cursor to the specified position, snapping back to nearest edge
;; or corner if necessary. Redraws the screen
(define/contract (move-cursor! x y)
  (-> integer? integer? void?)
  (define (get-in-range low high val)
    (cond
      [(< val low) low]
      [(> val high) high]
      [else val]))
  (define newx (get-in-range 0 (- (sref 'xpx) 1) x))
  (define newy (get-in-range 0 (- (sref 'ypx) 1) y))
  (sset! 'cursor-x newx)
  (sset! 'cursor-y newy)
  (paint!))

(define/contract (handle-ke! ke)
  (-> (is-a?/c key-event%) void?)
  (define x (sref 'cursor-x))
  (define y (sref 'cursor-y))
  (match (send ke get-key-code)
    [#\h (move-cursor! (- x 1) y)]
    [#\j (move-cursor! x (+ y 1))]
    [#\k (move-cursor! x (- y 1))]
    [#\l (move-cursor! (+ x 1) y)]
    [#\v (sset! 'cursor-visible? (not (sref 'cursor-visible?)))
         (paint!)]
    [_ (void)])
  (void))
          

(init-painter!
  handle-ke!
  (+ (* (sref 'xpx) (sref 'pxwd)) 100)
  (max 100 (* (sref 'ypx) (sref 'pxwd)))
  "girffetest")

(for ([mk (list mk-pic-background mk-pic-pixels mk-pic-cursor-info mk-pic-cursor)])
  (add-pic! (mk (sref 'xpx) (sref 'ypx) (sref 'pxwd))))

(paint!)
(printf "painted\n")
