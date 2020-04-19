#lang racket
(require algebraic/racket/base/forms)
(define-syntax m-fib
    (macro [n:nat #:if (< (var n) 2) 1]
           [n:nat (+ (m-fib #,(- (var n) 1))
                     (m-fib #,(- (var n) 2)))]))
;; (let ([a 7]) (m-fib a))
;;eval:126.0: m-fib: expected exact-nonnegative-integer
;;  at: a
;;  in: (m-fib a)
;;> (eval-syntax #`(list #,@(for/list ([n 7]) #`(m-fib #,n))))
;;'(1 1 2 3 5 8 13)

(let fac ([(X n) (X 10)])
    (if (zero? n)
        1
        (* n (fac (X (sub1 n))))))
;;3628800

;;If the pattern contains a delimited ., the final patt is matched against the argument’s tai
((φ (a . b) (list a b)) '(1))
;;'(1 ())

(define-syntax m-power
    (macro*
      [(0 _) 1]
      [(1 x) x]
      [(n:nat x) (* x (m-power #,(- (var n) 1) x))]))

(define-syntax m-power3 (μ y (m-power 3 y)))

(map (φ x (m-power3 x)) '(0 1 2 3 4 5 6))
;;'(0 1 8 27 64 125 216)

(letrec-values
      ([((X is-even?) (Y is-odd?))
        (id (X (φ n ((|| zero? (.. is-odd? sub1)) n)))
            (Y (φ n ((|| (<< = 1) (.. is-even? sub1)) n))))])
    (is-odd? 11))
;;#t
