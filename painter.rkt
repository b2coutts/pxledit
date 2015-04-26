#lang racket
;; code that intelligently handles the drawing of pictures based on dependencies. Each picture has a
;; rectangular space in which it can draw, and state keys on which it depends. Moreover, the
;; pictures are ordered, which determines which pictures are drawn on top of each other. An picture
;; is redrawn when one of the following conditions holds:
;;   (1) It depends on state which is dirty
;;   (2) Its drawing area intersects the drawing area of an picture for which (1) holds
;; Splitting elements up into multiple pictures (and making state dependencies more granular) allows
;; for more efficient drawing.

(provide (struct-out picture)
  init-painter!
  add-pic!
  remove-pic!
  paint!
)

(require "state.rkt")

(require racket/gui/base data/gvector
         (except-in 2htdp/image make-color make-pen)
         (only-in mrlib/image-core render-image))

(define-struct/contract picture (
  [key any/c]   ;; key of the picture, used for removal; should be unique
  [x1 integer?] ;; top-left corner of its drawing space
  [y1 integer?]
  [x2 integer?] ;; bottom-right corner of its drawing space
  [y2 integer?]
  [deps set?] ;; set of the state keys on which this depends
  [priority real?] ;; larger priority pictures are drawn last (over other pictures)
  [dense boolean?] ;; if #t, pictures underneath this need not be redrawn when this is
  [drawfn (-> image?)] ;; function which produces an image to draw
) #:transparent)

;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GLOBAL STATE @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; the frame
(define/contract frm
  (or/c (is-a?/c frame%) void?)
  (void))

;; the canvas and drawing context
(define/contract cvs
  (or/c (is-a?/c canvas%) void?)
  (void))

;; hash of keys to pictures
(define/contract pics
  (hash/c any/c picture?)
  (make-hash))

;; hash of sets; (hash-ref pdeps k) contains the keys of the pictures which depend on (hash-ref
;; pics k)
(define/contract pdeps
  (hash/c any/c set-mutable?)
  (make-hash))

;; initializes the entire module; sets up drawing context and global state
(define/contract (init-painter! on-char-fn width height title)
  (-> (-> (is-a?/c key-event%) void?) integer? integer? string? void?)
  (unless (and (void? frm) (void? cvs))
    (error "init-painter! called twice"))
  ;; TODO: assuming that the paint-callback doesn't actually need to redraw everything
  (set! frm (new (class frame%
                  (super-new)
                  (define/augment (on-close)
                    (custodian-shutdown-all (current-custodian))))
                 [label title]))
  (set! cvs (new (class canvas%
                  (super-new)
                  (define/override (on-char ke)
                    (on-char-fn ke)
                    (send cvs refresh)))
                 [parent frm]
                 [paint-callback paint-cb!]))
  (send cvs min-client-width width)
  (send cvs min-client-height height)
  (send frm show #t))

;; helper function; determines whether a "depends on" b, i.e., a needs to be redrawn if b is redrawn
(define/contract (depends-on? a b)
  (-> picture? picture? boolean?)
  (match-define (picture ak ax1 ay1 ax2 ay2 adeps apr adns afn) a)
  (match-define (picture bk bx1 by1 bx2 by2 bdeps bpr bdns bfn) b)
  (and (<= ax1 bx2) (<= bx1 ax2) (<= ay1 by2) (<= by1 ay2) ;; non-empty intersection
       (or (and (> apr bpr) (not adns))
           (and (> bpr apr) (not bdns)))))

(define/contract (add-pic! pic)
  (-> picture? void?)
  (when (hash-has-key? pics (picture-key pic))
    (error "add-pic! given duplicate key: ~s" (picture-key pic)))
  (define pdep (list->mutable-set (filter (lambda (k) (depends-on? (hash-ref pics k) pic))
                                          (hash-keys pics))))
  (hash-set! pdeps (picture-key pic) pdep)
  (for ([k (in-hash-keys pics)])
    (when (depends-on? pic (hash-ref pics k))
      (set-add! (hash-ref pdeps k) (picture-key pic))))
  (hash-set! pics (picture-key pic) pic))

(define/contract (remove-pic! key)
  (-> any/c void?)
  (when (not (hash-has-key? pics key))
    (error "remove-pic! key does not exist: ~s" key))
  (for ([k (in-hash-keys pics)]) ;; remove key from all other pdeps
    (set-remove! (hash-ref pdeps k) key))
  (hash-remove! pics key)
  (hash-remove! pdeps key))

;; helper function; adds (the keys of) all pictures that depend on (hash-ref pics key) to deps
(define/contract (add-deps! deps key)
  (-> set-mutable? any/c void?)
  (for ([pdep (in-set (hash-ref pdeps key))])
    (unless (set-member? deps pdep)
      (set-add! deps pdep)
      (add-deps! deps pdep))))
  
;; helper function which draws a picture
(define/contract (draw-pic! cvs dc pic)
  (-> (is-a?/c canvas%) (is-a?/c dc<%>) picture? void?)
  (match-define (picture k x1 y1 x2 y2 deps pr dns fn) pic)
  (define img (fn))
  (when (or (> (image-width img) (- x2 x1)) (> (image-height img) (- y2 y1)))
    (error "draw!: image given by ~s exceeds its bounds!" k))
  (render-image img dc x1 y1))

;; function which draws all necessary pictures
(define/contract (paint!)
  (-> void?)
  (send cvs refresh))

;; callback for the paint-callback of canvas%; TODO: should this redraw everything?
(define/contract (paint-cb! cvs-arg dc-arg)
  (-> (is-a?/c canvas%) (is-a?/c dc<%>) void?)
  (printf "foobar\n")
  (define dset (get-dirty)) ;; set of dirty state variables
  (define nlst (filter (lambda (k) (not (empty? (set-union (picture-deps (hash-ref pics k)) dset))))
                       (hash-keys pics)))
  (define deps (apply mutable-set nlst))
  (for ([k (in-set deps)]) ;; add all dependencies of pictures that need to be drawn
    (add-deps! deps k))
  (define todraw (sort (map (curry hash-ref pics) (set->list deps)) < #:key picture-priority))

  (for ([pic todraw]) (draw-pic! cvs-arg dc-arg pic)))
