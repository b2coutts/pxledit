#lang racket
;; module for handling key-events without blocking on drawing

(provide init-handler!)

(require racket/gui/base)

;; helper function, gets all currently available thread messages
(define/contract (thread-receive-all)
  (-> list?)
  (match (thread-try-receive)
    [#f '()]
    [val (cons val (thread-receive-all))]))

;; if ke represents an important key-event, return the key-event, else return #f
(define (important? ke)
  (match ke
    ['paint #f]
    [(cons 'key-event ke) (match (send ke get-key-code)
      ['release #f]
      [_ ke])]))

;; master thread, which handles key-event requests and paint-callback! requests
(define/contract (master-handler key-handler paint-cb)
  (-> (-> (is-a?/c key-event%) void?) (-> void?) void?)
  (define (loop)
    (define queue (thread-receive-all))
    (printf "DEBUG: queue is: ~s\n" queue)
    (cond
      [(empty? queue) (sync (thread-receive-evt))]
      [else (define imps (filter-map important? queue))
            (unless (and (empty? imps) (not (member 'paint queue)))
              (for ([ke imps])
                    (key-handler ke))
                  (paint-cb))])
    (loop))
  (loop))

;; produces a key handler that can be supplied as the paint-callback! of canvas%. 
(define/contract (init-handler! key-handler paint-cb)
  (-> (-> (is-a?/c key-event%) void?) (-> void?)
      (values (-> (is-a?/c key-event%) void?) (-> (is-a?/c canvas%) (is-a?/c dc<%>) void?)))
  (define thd (thread (thunk (master-handler key-handler paint-cb))))
  (values
    (lambda (ke) (void (thread-send thd (cons 'key-event ke))))
    (lambda (cv dc) (void (thread-send thd 'paint)))))
