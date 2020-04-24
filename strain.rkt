#lang rosette
(require rosette/query/debug rosette/lib/render)
 
(require rosette/lib/synthax)
 

(define (θ 0))
(define (r 0))

(define (tan-radius (tan θ r) ;; based on theta, the E1

(define (ε1 tan-radius)) ;; longitudinal strain ΔL/L
(define (θ-after (if (> θ 180)(- θ 180))))
;;law of cosines deployed below
(define (ε2 (sqrt (- (* 2(expt 2 r) (*(* 2 (* a b)) (cos θ-after (atan (/ r r))))))))) ;; Transverse strain ΔD/D 
(define (poissons-ratio (/ ε2 ε1)))

;; find system of equations for θ
;;E1 = Ex Cos^2(θ1) + Eysin^2(θ) + δxysin(θ)cos(θ)
;;E2 = Ex Cos^2(θ1) + Eysin^2(θ) + δxysin(θ)cos(θ)
;;E3 = Ex Cos^2(θ1) + Eysin^2(θ) + δxysin(θ)cos(θ)

;given two circles like so:
;;(x-x1)^2 + (y-y1)^2 = r^2
;;(x-x2)^2 + (y-y2)^2 = r^2

;;returns these results
;;x = (-sqrt(-(y1-y2)^2 (x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2) (-4 r^2+x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2))+x1^3-x1^2 x2-x1 x2^2+x1 y1^2-2 x1 y1 y2+x1 y2^2+x2^3+x2 y1^2-2 x2 y1 y2+x2 y2^2)/(2 (x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2)) 
;;y = (x1 sqrt(-(y1-y2)^2 (x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2) (-4 r^2+x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2))-x2 sqrt(-(y1-y2)^2 (x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2) (-4 r^2+x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2))+x1^2 y1^2-x1^2 y2^2-2 x1 x2 y1^2+2 x1 x2 y2^2+x2^2 y1^2-x2^2 y2^2+y1^4-2 y1^3 y2+2 y1 y2^3-y2^4)/(2 (y1-y2) (x1^2-2 x1 x2+x2^2+y1^2-2 y1 y2+y2^2)) 

;;solving matrixes using racket

(define M (matrix [x1 y1 z1] [x2 y2 y3] [x3 y3 z3])))
(define B0 (col-matrix [x1 y1 z1]))
(define B1 (col-matrix [x2 y2 z2]))
(define B2 (col-matrix [x3 y3 z3]))
(define situation (matrix-cols (matrix-solve M (matrix-augment (list B0 B1 B2)))))

(define (poly x y)
 (+ (expt (- x x1) 2) (expt (- y y1) 2)))
 
(define (factored xp yp)
 (+ (expt (- xp x2) 2) (expt (- yp y2) 2)))
 
(define (same p f x)
 (assert (= ((* r r) x) ((* r r) x))))

(define solve-for-x (x1 y1 x2 y2)
  (solve p f x))
;(list (array #[#[4] #[-5]]) (array #[#[2] #[1]]))
(require containment-patterns rackunit)

(define εx (check-equal?
                (match situation
                   [(⋱ `(target. (matrix-solve M B1))) target])
                `x))

(define εz (check-equal?
                (match situation
                   [(⋱ `((matrix-solve M B1), target)) target])
                `z))

(define εy (check-equal?
                (match situation
                   [(⋱ `((matrix-solve M B0), target)) target])
                `y))

(define E1 (+ (/ (+ εy εx) 2)  (sqrt  (+ (expt (/ (- εy εx) 2) 2) (expt(/ εz 2) 2)))))

(define E2 ((- (/ (+ εy εx) 2)  (sqrt  (+ (expt (/ (- εy εx) 2) 2) (expt(/ εz 2) 2))))))

(define E3 ((* (/ 0.333 (1 - 0.3333)) (+ E1 E2))))

(define o1 (* (/ 10.4 (- 1 (* 0.333 0.333))) (+ E1 (* 0.333 E2)))) ;; shifts to the right
(define o2 (0))
(define o3 (* (/ 10.4 (- 1 (* 0.333 0.333))) (+ E2 (* 0.333 E1)))) ;; shifts to the left
  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter
          (lambda (position) (safe? position))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                      (cons new-row rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (define (safe? position)
    (safe-iter? (car position) 1 (cdr position)))
  (define (safe-iter? fst n rest-position)
    (cond ((null? rest-position) true)
          ((= fst (car rest-position)) false)
          ((= (abs (- fst (car rest-position))) n) false)
          (else (safe-iter? fst (+ n 1) (cdr rest-position)))))
  (queen-cols board-size))
