#lang racket

(require "painter.rkt" "state.rkt" "pictures.rkt" "util.rkt" "image.rkt" "handler.rkt")

(require racket/gui/base data/gvector
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

;; parse cmd line arguments, import an image if one is given
(define img-x-size (make-parameter 20))
(define img-y-size (make-parameter 20))
(define img-px-width (make-parameter 16))

(define img-filename (command-line
  #:program "pxledit"
  #:once-each
    [("-x" "--xsize") xsize
                      "Edit an image xsize pixels wide"
                      (img-x-size (string->number xsize))]
    [("-y" "--ysize") ysize
                      "Edit an image ysize pixels tall"
                      (img-y-size (string->number ysize))]
    [("-p" "--pixel-width") pw
                            "Display each pixel with a pw-by-pw square on screen"
                            (img-px-width (string->number pw))]
  #:args (filename)
  filename))

(sset! 'xpx (img-x-size))
(sset! 'ypx (img-y-size))
(sset! 'pxwd (img-px-width))
(sset! 'filename img-filename)
(sset! 'cursor-x 0) ;; x- and y- positions of the cursor, 0-indexed
(sset! 'cursor-y 0)
(sset! 'colors (make-vector (* (sref 'xpx) (sref 'ypx)) white)) ;; the bitmap colours
(sset! 'cursor-visible? #t)
(sset! 'current-brush 1)  ;; the current brush (colour) being used
(sset! 'brushes (make-vector 10 black))
(sset! 'undo-stack '())

(when (file-exists? img-filename)
  (define perms (file-or-directory-permissions img-filename))
  (cond
    [(not (member 'read perms))
      (error (format "ERROR: you do not have permission to read from ~a" img-filename))]
    [(not (member 'write perms))
      (error (format "ERROR: you do not have permission to write to ~a" img-filename))]
    [else (define-values (vec width height) (read-pixels-from-file img-filename))
          (sset! 'xpx width)
          (sset! 'ypx width)
          (sset! 'colors vec)]))

;; TODO: temporary; makes the default image more interesting
(when #f
  (for ([x (sref 'xpx)])
    (for ([y (sref 'ypx)])
      (vector-set! (srefd! 'colors) (+ (* y (sref 'xpx)) x) (color 0 (* x 10) (* y 10) 255)))))

;; installs all pictures to painter, using the given pixel counts/size
(define (install-pictures!)
  (for ([mk (list mk-pic-background mk-pic-pixels mk-pic-cursor-info mk-pic-cursor
                  mk-pic-filename)])
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
  (sset! 'cursor-y newy))

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
  (define idx (+ (* y (sref 'xpx)) x))
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
    [#\c (sset! 'cursor-visible? (not (sref 'cursor-visible?)))]

    ;; read colour under cursor into current brush
    [#\q (vector-set! (srefd! 'brushes) (sref 'current-brush)
                      (vector-ref (sref 'colors) (+ (* y (sref 'xpx)) x)))]

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
      (sset! 'current-brush (- (char->integer code) 48))]

    ;; apply brush to current pixel
    [#\d (sset! 'undo-stack (cons (list x y (vector-ref (sref 'colors) idx)) (sref 'undo-stack)))
         (vector-set! (srefd! 'colors) idx (current-brush-color))]

    ;; zoom in/out
    [#\+ (clear-pics!)
         (sset! 'pxwd (+ (sref 'pxwd) 1))
         (install-pictures!)]
    [#\- (clear-pics!)
         (sset! 'pxwd (- (sref 'pxwd) 1))
         (install-pictures!)]

    ;; save the current image
    [#\s (write-pixels-to-file (sref 'colors) (sref 'xpx) (sref 'ypx) (sref 'filename))]

    ;; undo
    [#\u (match (sref 'undo-stack)
          [(cons (list ux uy color) as)
            (sset! 'undo-stack as)
            (move-cursor! ux uy)
            (vector-set! (srefd! 'colors) (+ (* uy (sref 'xpx)) ux) color)]
          [_ (void)])]
    [_ (void)])
  (void))

;; construct handlers for events/painting
(define-values (key-handler paint-handler) (init-handler! handle-ke! paint!))
          
(init-painter!
  key-handler
  paint-handler
  (+ (* (sref 'xpx) (sref 'pxwd)) 100)
  (max 100 (* (sref 'ypx) (sref 'pxwd)))
  "girffetest")

(install-pictures!)

(paint!)
(printf "pxledit initialized\n")
