#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse))

(define-conventions xyz-as-ids
  [x id] [y id] [z id])

(define-syntax (swap stx)
  (syntax-parse stx #:conventions (xyz-as-ids)
    [(_ x y) #'(let ([t x])
                 (set! x y)
                 (set! y t))]))
(define-syntax define-syntax-rule
  (lambda (stx)
    (let-values ([(err) (lambda (what . xs)
                          (apply raise-syntax-error
                                 'define-syntax-rule what stx xs))])
      (syntax-case stx ()
        [(dr (name . pattern) template)
         (identifier? #'name)
         (syntax/loc stx
           (define-syntax name
             (lambda (user-stx)
               (syntax-case** dr #t user-stx () free-identifier=? #f
                 [(_ . pattern) (syntax-protect (syntax/loc user-stx template))]
                 [_ (pattern-failure user-stx 'pattern)]))))]
        [(_ (name . ptrn) tmpl)         (err "expected an identifier" #'name)]
        [(_ (name . ptrn))              (err "missing template")]
        [(_ (name . ptrn) tmpl etc . _) (err "too many forms" #'etc)]
        [(_ head . _)                   (err "invalid pattern" #'head)]))))


(require racket/generator)
(define evens
  (generator
   ()
   (let loop ([x 1])
     (if (zero? (modulo x 2))
         (begin
           (yield x)
           (loop (+ 1 x)))
         (loop (+ 1 x))))))

;1. (reset val) => val
;2. (reset E[(shift k expr)]) => (reset ((lambda (k) expr)
;                                     (lambda (v) (reset E[v]))))
    ; where E has no reset

(require racket/control)
(define (generate-one-at-a-time lst)
  (define k #f)
  (define (go)
    (if k
        (k)
        (reset (let ()
             (for-each
              (lambda (x)
                (shift cur-k
                       (let ()
                         (set! k cur-k)
                         x)))
              lst)
             ‘done))))
  go)
;(define my-gen (generate-one-at-a-time '(1 2 3 4)))
; (my-gen) => 1 ... 2 ... 3... 4

;(expand-expression
;   #'(let ([x 42])
;       (letrec-syntax ([y (make-rename-transformer #'z)]
;                       [z (make-rename-transformer #'x)])
;         (+ y 3))))
;#<syntax (let-values (((x) '42))
;           (letrec-values ()
;             (#%plain-app + x '3)))>