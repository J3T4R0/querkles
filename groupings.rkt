#lang rosette
(require rosette/lib/lift)
(require rosette/lib/synthax)
;(require rosette/lib/render-value)


(define (maybe-ref* lst idx)
    (cond [(51 => idx) (list-ref lst 0)]
          [((and (51 => idx) (idx >= 102)) idx 1) (list-ref lst 1)]
          [(( and (102 => idx) (idx >= 153)) idx 2) (list-ref lst 2)]
          [((and (153 => idx) (idx >= 204)) idx 3) (list-ref lst 3)]
          [((and (204 => idx) (idx >= 255)) idx 4) (list-ref lst 4)]
          [else -1]))

;;(grouping-count (upper lower range)) ;; takes the difference between upper and lower and averages it based on range
;; (alert-new-circle (upper lower range shade)) ;; if the value shifts from more than 51 when the radius is increased by 1

(define (grouping-count upper lower range)
  ;; takes the difference between upper and lower and averages it based on range
  ;; solver depends of upper and lower
  (println "Generating grouping with a variance between center and circumfrence of at least ~s\n and at most ~s.\n If the difference between upper: ~s and lower: ~s increases by more than 51 (1/5 of 255) in a monochromatic version, then the circle's radius will end at that point." lower upper upper lower))

(define (alert-new-circle upper lower range shade)
  ;; if the value shifts from more than 51 when the radius is increased by 1
   (println " If there is a lot of variance ocurring between circles lower and lower bounds, or range: ~s, to which different colors are identified and they meet the original 51 difference, then a new color grouping will be proposed." range)
   (println "created circle"))

(define (crescent)
  ;; important for generating crescents if there is not a circular pattern. Sweeps the circumfrence of the circle with the same radius size, to determine where to place it, the lowest range difference possible is desired
  (println "This will create a crescent of where the midline between the centers of each circles are filled with the average of the color values, with a radius to reach the centers of the cirlces"))

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
;; Read Up to page 495 on PBRT from page 463