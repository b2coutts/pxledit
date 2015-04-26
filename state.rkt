#lang racket
;; code for handling a state

(provide sref srefd! dirty? sset! dset! dset-all! get-dirty)

(define-struct/contract state (
  [data hash?] ;; data store, mapping keys to values
  [dirty set-mutable?] ;; set of the keys which are dirty (i.e., modified since last draw)
) #:transparent)

(define st (state (make-hash) (mutable-set)))

(define key? (curry hash-has-key? (state-data st)))
(define val? any/c)

;; lookup a value in the state
(define/contract (sref key)
  (-> any/c val?)
  (unless (hash-has-key? (state-data st) key)
    (error (format "sref: key does not exist: ~s" key)))
  (hash-ref (state-data st) key))

;; like sref, but sets the dirty flag
(define/contract (srefd! key)
  (-> any/c val?)
  (set-add! (state-dirty st) key)
  (hash-ref (state-data st) key))

;; lookup whether or not a key is dirty
(define/contract (dirty? key)
  (-> key? boolean?)
  (set-member? (state-dirty st) key))

;; add/update a key/value to the state
(define/contract (sset! key val)
  (-> any/c val? void?)
  (hash-set! (state-data st) key val)
  (set-add! (state-dirty st) key))

;; set/unset a dirty flag
(define/contract (dset! key drty)
  (-> key? boolean? void?)
  ((if drty set-add! set-remove!) (state-dirty st) key))

;; sets all dirty flags to the given value
(define/contract (dset-all! drty)
  (-> boolean? void?)
  (cond
    [drty (set-union! (state-dirty st) (list->mutable-set (hash-keys (state-data st))))]
    [else (set-clear! (state-dirty st))]))

;; produces a hash of all dirty keys
(define/contract (get-dirty)
  (-> set-mutable?)
  (state-dirty st))
