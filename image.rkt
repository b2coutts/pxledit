#lang racket
;; code for reading/writing image files

(provide write-pixels-to-file read-pixels-from-file)

(require racket/draw (except-in 2htdp/image make-color make-pen))

;; infers an image filetype from the file extension
(define/contract (get-image-filetype filename)
  (-> path-string? (or/c 'png 'jpeg 'xbm 'xpm 'bmp))
  (match (last (string-split filename "."))
    [(or "jpg" "jpeg") 'jpeg]
    [_ 'png]))

;; outputs a vector of a form like (sref 'pixels) to the given filename
(define/contract (write-pixels-to-file vec width height filename)
  (-> vector? integer? integer? path-string? void?)
  (define bmp (make-object bitmap% width height #f #t))
  (define pxls (make-bytes (* 4 width height)))
  (for ([y height])
    (for ([x width])
      (match-define (color r g b a) (vector-ref vec (+ (* y width) x)))
      (bytes-set! pxls (+ (* 4 (+ (* y width) x)) 0) a)
      (bytes-set! pxls (+ (* 4 (+ (* y width) x)) 1) r)
      (bytes-set! pxls (+ (* 4 (+ (* y width) x)) 2) b)
      (bytes-set! pxls (+ (* 4 (+ (* y width) x)) 3) g)))
  (send bmp set-argb-pixels 0 0 width height pxls)
  (send bmp save-file filename (get-image-filetype filename))
  (void))

;; inverse of the above; produces the vector, width, and height of the image
(define/contract (read-pixels-from-file filename)
  (-> path-string? (values vector? integer? integer?))
  (define bmp (make-object bitmap% 1 1 #f #t 1.0))
  (send bmp load-file filename 'unknown #f #t)
  (define width (send bmp get-width))
  (define height (send bmp get-height))
  (define pxls (make-bytes (* 4 width height)))
  (send bmp get-argb-pixels 0 0 width height pxls)
  (define vec (make-vector (* height width)))
  (for ([x width])
    (for ([y height])
      (match-define (list a r b g)
        (map (lambda (foo) (bytes-ref pxls (+ (* 4 (+ (* y width) x)) foo))) '(0 1 2 3)))
      (vector-set! vec (+ (* y width) x) (color r g b a))))
  (values vec width height))
