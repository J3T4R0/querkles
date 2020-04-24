#lang racket

(require (for-syntax racket/syntax)
         syntax/parse/define)

(provide define/cross-phase)

(define-simple-macro (define/cross-phase x:id e:expr)
  #:with topic-mod-name (generate-temporary 'cross-phase-topic-key)
  (begin
    (module topic-mod-name '#%kernel
      (#%declare #:cross-phase-persistent)
      (#%provide topic)
      (define-values [topic] (gensym "cross-phase")))
    (require 'topic-mod-name)
    (define x (make-cross-phase topic (λ () e)))))

(define root-logger (current-logger))

(define (make-cross-phase topic thunk)
  (define receiver (make-log-receiver root-logger 'debug topic))
  (define chan (make-channel))
  (define executor (make-will-executor))

  (let ()
    (define canary (gensym 'canary))
    (will-register executor canary (λ (v) 'collected))
    (log-message root-logger 'debug topic ""
                 (vector-immutable canary chan) #f)
    (let loop ()
      (match (sync receiver)
        [(vector _ _ (vector _ (== chan eq?)) _)
         (void)]
        [_
         (loop)])))

  (define execute-evt (wrap-evt executor will-execute))
  (define result (let loop ([n 0])
                   (sleep)
                   (or (sync/timeout 0 chan execute-evt)
                       (begin
                         (collect-garbage (if (< n 3) 'minor 'major))
                         (loop (add1 n))))))

  (match result
    [(vector _ value)
     value]
    ['collected
     (define value (thunk))
     (thread
      (λ ()
        (let loop ()
          (match (sync receiver)
            [(vector _ _ (vector canary chan) _)
             (thread (λ () (channel-put chan (vector-immutable canary value))))
             (loop)]))))
     value]))