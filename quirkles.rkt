#lang rosette
(require rosette/lib/lift)
;(require rosette/lib/render-value)
(require (only-in racket [string-length racket/string-length] string?))
(require "gui.rkt")
 
;(define-lift string-length [(string?) racket/string-length])

;(require (only-in racket build-list))

;(define limit 1000)

;(define 
 
;(define (slow xs)
; '(and (= (length xs) limit) (car (map add1 xs))))

;(define (fast xs)
 ;(for/all ([xs xs]) (slow xs)))
 
;(define ys (build-list limit identity))
 
;(define-symbolic a boolean?)
 
;(define xs (if a ys (cdr ys)))


(define-symbolic* x y integer?)

(define (list-set* lst idx val)
    (match lst
      [(cons x xs)
       (cons (if (= idx 0) val x)
             (list-set* xs (- idx 1) val))]
      [_ lst]))

 (define-values (width height) (values 5 5))


(define grid/flat
    (make-vector (* width height) #f))

(vector-set! grid/flat (+ (* y width) x) 'a)

;(render-value/snip (list (hash 1 2 3 4)
 ;                          (set 1 2 3))
  ;                   #:handler (λ (value rec)
   ;                              (cond
    ;                               [(hash? value)
     ;                               (append*
      ;                               '([(emph "Kind: ") "hash"])
       ;                             (for/list ([(k v) (in-hash value)])
       ;                                `(#:gap
        ;                                 [,(rec k)]
         ;                                [,(rec v)])))]
          ;                         [else #f])))

  ;(define (maybe-ref* lst idx)
  ;  (cond [(51<= idx 0) (list-ref lst 0)]
   ;       [((51=> and >= 102) idx 1) (list-ref lst 1)]
    ;      [((102=> and >= 153) idx 2) (list-ref lst 2)]
     ;     [((153=> and >= 204) idx 3) (list-ref lst 3)]
      ;    [((204=> and >= 255) idx 4) (list-ref lst 4)]
       ;   [else -1]))

 ; (maybe-ref* '(5 6 7) idx)

 (define-generics viewable (view viewable))

(struct circle (radius)
    #:transparent
    #:methods gen:viewable
    [(define (view self) (circle-radius self))])

(define-symbolic b boolean?)

(define sol (solve (assert (= (point-x p) 3))))
(evaluate p sol)
;#(struct:point 3 4)

;;;;;;;;;;;;;;API DEFINITIONS:;;;;;;;;;;;;
 ;; (gen-radius r)
 ;; (set-center (point x y))
 ;; (draw-circles (r (point x y)))
 ;; (assign-value (#struct:cirlce value)
 ;; (determine-value (gr
 ;; (grouping-count (upper lower range)) ;; takes the difference between upper and lower and averages it based on range
 ;; (alert-new-circle (upper lower range shade)) ;; if the value shifts from more than 51 when the radius is increased by 1
 ;; (crescent () ;; important for generating crescents if there is not a circular pattern. Sweeps the circumfrence of the circle with the same radius size, to determine where to place it, the lowest range difference possible is desired


(define p (if b (point 1 2) (point 3 4))) ; p holds a symbolic structure

;(define p (if b (square 2) (circle 3))) ; p holds a symbolic structure
;(add-drawing (view 10))

(define (div2 x)
    ([choose bvshl bvashr bvlshr bvadd bvsub bvmul] x (?? (bitvector 8))))

(define-symbolic i (bitvector 8))

 (print-forms
   (synthesize #:forall (list i)
               #:guarantee (assert (equal? (div2 i) (bvudiv i (bv 2 8))))))

; The following query has a solution because the second
; clause of LA2 creates two independent (??) holes.
(define-synthax LA2
  ([(_ e) (* e (??))]
   [(_ e1 e2) (+ (* e1 (??)) (* e2 (??)))]))
 
(define sol2
  (synthesize
   #:forall (list x y)
   #:guarantee (assert (= (LA2 x y) (+ (* 2 x) y)))))

 (define-symbolic x y integer?)

(define sol
    (solve (begin (assert (not (= x y)))
                  (assert (< (abs x) 10))
                  (assert (< (abs y) 10))
                  (assert (not (= (poly x) 0)))
                  (assert (= (poly x) (poly y))))))

(letrec ([adder (lambda (vs n)
                    (if (null? vs)
                        (list)
                        (cons (+ (car vs) n) (adder (cdr vs) n))))])
    (adder '(1 2 3) x))


  