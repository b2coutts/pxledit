#lang racket

(provide mk-pic-background mk-pic-pixels mk-pic-cursor-info mk-pic-cursor mk-pic-filename get-color)

(require "painter.rkt" "state.rkt" "util.rkt")

(require racket/gui/base data/gvector lang/posn
         (except-in 2htdp/image make-color make-pen))

;; get the color of the pixel at (i,j)  (ith column, jth row)
(define/contract (get-color xpx i j)
  (-> integer? integer? integer? color?)
  (vector-ref (sref 'colors) (+ (* j (sref 'xpx)) i)))

(define/contract (pixel-fn xpx ypx pxwd)
  (-> integer? integer? integer? image?)
  ;; TODO: build coords list (see workspace 8)
  (define-values (imgs posns)
    (for/fold ([imgs '()]
               [posns '()])
              ([i xpx])
      (define-values (imglst posnlst)
        (for/lists (l1 l2)
                   ([j ypx])
          (values (rectangle pxwd pxwd "solid" (get-color xpx i j))
                  ;; TODO: why is this 5 necessary? It makes me uncomfortable :(
                  (make-posn (* pxwd i) (* pxwd j)))))
      (values (append imglst imgs) (append posnlst posns))))
  (place-images/align imgs posns "left" "top"
    (rectangle (* pxwd (sref 'xpx)) (* pxwd (sref 'ypx)) "solid" "gray")))

(define/contract (mk-pic-pixels xpx ypx pxwd)
  (-> integer? integer? integer? picture?)
  (picture
    'pixels                           ;; key
    1                                 ;; x1
    1                                 ;; y1
    (* xpx pxwd)                      ;; x2
    (* ypx pxwd)                      ;; y2
    (set 'colors)                     ;; deps
    0.0                               ;; priority
    #t                                ;; dense
    (thunk (pixel-fn xpx ypx pxwd)))) ;; drawfn

(define/contract (mk-pic-background xpx ypx pxwd)
  (-> integer? integer? integer? picture?)
  (picture
    'background
    1
    1
    (+ (* xpx pxwd) 100)
    (max (* ypx pxwd) 100)
    (set)
    -999.0
    #t
    (const (rectangle (+ (* pxwd xpx) 100) (max (* pxwd ypx) 100) "solid" "black"))))

(define/contract (mk-pic-cursor xpx ypx pxwd)
  (-> integer? integer? integer? picture?)
  (define empty-rec (rectangle (* pxwd xpx) (* pxwd ypx) "solid" (color 0 0 0 0)))
  (define cwidth (floor (* pxwd 0.3)))
  (define (cursor-img col)
    (overlay
      (circle (- cwidth 1) "solid" col)
      (circle cwidth "solid" (invert-color col))
      (rectangle pxwd pxwd "solid" (color 0 0 0 0))))
  (picture
    'cursor
    1
    1
    (* xpx pxwd)
    (* ypx pxwd)
    (set 'cursor-x 'cursor-y 'cursor-visible? 'current-brush 'brushes)
    1.0
    #f
    (thunk (define col (vector-ref (sref 'brushes) (sref 'current-brush)))
           (if (sref 'cursor-visible?)
               (place-image/align (cursor-img col) (* (sref 'cursor-x) (sref 'pxwd))
                                  (* (sref 'cursor-y) (sref 'pxwd)) "left" "top" empty-rec)
               empty-rec))))

(define/contract (mk-pic-cursor-info xpx ypx pxwd)
  (-> integer? integer? integer? picture?)
  (picture
    'cursor-info
    (+ (* xpx pxwd) 1)
    1
    (+ (* xpx pxwd) 100)
    120
    (set 'cursor-x 'cursor-y 'brushes 'current-brush)
    1.0
    #f
    (thunk (match-define (color r g b a) (vector-ref (sref 'brushes) (sref 'current-brush)))
           (above/align "left"
            (text (format "(~a,~a)" (sref 'cursor-x) (sref 'cursor-y)) 14 'white)
            (text (format "R: ~a" r) 14 'red)
            (text (format "G: ~a" g) 14 'green)
            (text (format "B: ~a" b) 14 'blue)
            (text (format "A: ~a" a) 14 'white)))))

(define/contract (mk-pic-filename xpx ypx pxwd)
  (-> integer? integer? integer? picture?)
  (picture
    'filename
    (+ (* xpx pxwd) 1)
    150
    (+ (* xpx pxwd) 100)
    200
    (set 'filename)
    0.5
    #f
    (thunk (text (sref 'filename) 14 'white))))
