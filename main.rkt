#lang racket

(require "painter.rkt" "state.rkt" "pictures.rkt" "util.rkt")

(require racket/gui/base data/gvector
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

;; TODO: un-hardcode all of these values
(sset! 'pxwd 16)    ;; displayed width of a bmp pixel
(sset! 'xpx 20)     ;; number of columns in bmp
(sset! 'ypx 20)     ;; number of rows in bmp
(sset! 'cursor-x 0) ;; x- and y- positions of the cursor, 0-indexed
(sset! 'cursor-y 0)
(sset! 'colors (make-vector (* (sref 'xpx) (sref 'ypx)) white)) ;; the bitmap colours
(sset! 'cursor-visible? #t)
(sset! 'current-brush 1)  ;; the current brush (colour) being used
(sset! 'brushes (make-vector 10 white))

;; TODO: temporary; makes the default image more interesting
(when #f
  (for ([x (sref 'xpx)])
    (for ([y (sref 'ypx)])
      (vector-set! (srefd! 'colors) (+ (* y (sref 'xpx)) x) (color 0 (* x 10) (* y 10) 255)))))

;; installs all pictures to painter, using the given pixel counts/size
(define (install-pictures!)
  (for ([mk (list mk-pic-background mk-pic-pixels mk-pic-cursor-info mk-pic-cursor)])
    (add-pic! (mk (sref 'xpx) (sref 'ypx) (sref 'pxwd)))))

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

;; helper function, gets the current color
(define/contract (current-brush-color)
  (-> color?)
  (vector-ref (sref 'brushes) (sref 'current-brush)))

;; increments the given colour value of the current brush by the given amount
(define/contract (inc-color! col amt)
  (-> (or/c 'red 'green 'blue 'alpha) integer? void?)
  (match-define (color r g b a) (current-brush-color))
  (vector-set! (srefd! 'brushes) (sref 'current-brush)
    (match col
      ['red   (color (modulo (+ r amt) 256) g b a)]
      ['green (color r (modulo (+ g amt) 256) b a)]
      ['blue (color r g (modulo (+ b amt) 256) a)]
      ['alpha (color r g b (modulo (+ a amt) 256))])))

(define/contract (handle-ke! ke)
  (-> (is-a?/c key-event%) void?)
  (define x (sref 'cursor-x))
  (define y (sref 'cursor-y))
  (define ctrl (send ke get-control-down))
  (define code (send ke get-key-code))
  (match code
    ;; navigate the area
    [#\h (move-cursor! (- x (if ctrl 10 1)) y)]
    [#\H (move-cursor! (- x 5) y)]
    [#\j (move-cursor! x (+ y (if ctrl 10 1)))]
    [#\J (move-cursor! x (+ y 5))]
    [#\k (move-cursor! x (- y (if ctrl 10 1)))]
    [#\K (move-cursor! x (- y 5))]
    [#\l (move-cursor! (+ x (if ctrl 10 1)) y)]
    [#\L (move-cursor! (+ x 5) y)]
    [#\0 (move-cursor! 0 y)]
    [#\$ (move-cursor! (sref 'xpx) y)]
    [#\( (move-cursor! x 0)]
    [#\) (move-cursor! x (sref 'ypx))]

    ;; toggle cursor visibility
    [#\c (sset! 'cursor-visible? (not (sref 'cursor-visible?)))
         (paint!)]

    ;; read colour under cursor into current brush
    [#\q (vector-set! (srefd! 'brushes) (sref 'current-brush)
                      (vector-ref (sref 'colors) (+ (* y (sref 'xpx)) x)))
         (paint!)]

    ;; adjust red amount
    [#\r (inc-color! 'red (if ctrl 50 1))]
    [#\R (inc-color! 'red 10)]
    [#\e (inc-color! 'red (if ctrl -50 -1))]
    [#\E (inc-color! 'red -10)]

    ;; adjust green amount
    [#\g (inc-color! 'green (if ctrl 50 1))]
    [#\G (inc-color! 'green 10)]
    [#\f (inc-color! 'green (if ctrl -50 -1))]
    [#\F (inc-color! 'green -10)]

    ;; adjust blue amount
    [#\b (inc-color! 'blue (if ctrl 50 1))]
    [#\B (inc-color! 'blue 10)]
    [#\v (inc-color! 'blue (if ctrl -50 -1))]
    [#\V (inc-color! 'blue -10)]

    ;; adjust alpha amount
    [#\a (inc-color! 'alpha (if ctrl 50 1))]
    [#\A (inc-color! 'alpha 10)]
    [#\z (inc-color! 'alpha (if ctrl -50 -1))]
    [#\Z (inc-color! 'alpha -10)]

    ;; change brush
    [(or #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (sset! 'current-brush (- (char->integer code) 48))
      (paint!)]

    ;; apply brush to current pixel
    [#\d (vector-set! (srefd! 'colors) (+ (* y (sref 'xpx)) x) (current-brush-color))
         (paint!)]

    ;; zoom in/out
    [#\+ (clear-pics!)
         (sset! 'pxwd (+ (sref 'pxwd) 1))
         (install-pictures!)
         (paint!)]
    [#\- (clear-pics!)
         (sset! 'pxwd (- (sref 'pxwd) 1))
         (install-pictures!)
         (paint!)]
    [_ (void)])
  (void))
          
(init-painter!
  handle-ke!
  (+ (* (sref 'xpx) (sref 'pxwd)) 100)
  (max 100 (* (sref 'ypx) (sref 'pxwd)))
  "girffetest")

(install-pictures!)

(paint!)
(printf "pxledit initialized\n")
