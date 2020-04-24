#lang rosette
(require rosette/lib/lift)
(require rosette/lib/synthax)
;(require rosette/lib/render-value)


(define (maybe-ref* lst idx)
    (cond [(51 => idx) (list-ref lst 0)] ;; almost black 
          [((and (51 => idx) (idx >= 102)) idx 1) (list-ref lst 1)] ;; twilight dark, more than just gray
          [(( and (102 => idx) (idx >= 153)) idx 2) (list-ref lst 2)] ;; contains noticeble dark 
          [((and (153 => idx) (idx >= 204)) idx 3) (list-ref lst 3)];; gray 
          [((and (204 => idx) (idx >= 255)) idx 4) (list-ref lst 4)] ;; for light gray shading or nonexistent monochrome values
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

 (define-symbolic radius range integer?)

(define (points-around circle radius)
  (for ([angle (angle <= 359)])
    (let ([+ angle 1]))
    (let ([x (* radius (cos angle))]))
    (let ([y (* radius (sin angle))])) ;; hypotenuse value (radius) times cos 50
    (let ([xarr (cons (trunc x) xarr)])) ;; become x and y pixel locations
    (let ([yarr (cons (trunc y) yarr)])))) ;; extract monochrome and color values from them

(require "colors.rkt" (quicksort partition))
;;imports (define (quicksort compare ls)
;;imports(define (partition left? ls)

(define (find-upper pixels)
  for ([i (i < (length pixels))])
    (let ([+ i 1]))
    (synthesize
     #:forall (list pixels)
     ((index-ref (quicksort (compare-monochrome-levels pixels sel) (filter (index-ref pixels #[(index-ref xarr i) (index-ref yarr i)]))) 0)))
   
(define (find-lower pixels)
  for ([i (i < (length pixels))])
    (let ([+ i 1]))
    (synthesize
     #:forall (list pixels)
     (index-ref (quicksort (compare-monochrome-levels pixels sel) (filter (index-ref pixels #[(index-ref xarr i) (index-ref yarr i)]))) 0)))

(define (find-variance pixels)
  (if (> (- (find-upper pixels) (find-lower)) 51) (create-elipse ))) ;; TODO 

(define (find-range pixels)
  (if (> (- (find-upper pixels) (get-monochrome pixel self))) (gen-circle-solver)))

(define sol
    (solve (begin (assert (not (> range 51)))
                  (if (> range 51) (gen-cirle radius range)))))

(letrec ([adder (lambda (vs n)
                    (if (null? vs)
                        (list)
                        (cons (+ (car vs) n) (adder (cdr vs) n))))])
    (adder '(1 2 3) x))

;; (define-symbolic radius range integer?)
;;find how to establish a cost function of minimal values over a sine function 
(define-synthax LA2
  ([(_ e) (* e (??))]
   [(_ e1 e2) (+ (* e1 (??)) (* e2 (??)))]))
 
  
(define (gen-cirlcle)
  (synthesize
   #:forall (list pixels)
   #:guarantee (assert (= (LA2 x y) (+ (* 2 x) y)))))
;; Read Up to page 495 on PBRT from page 463

;;to call in file execute these commands
;; (define (start-pixel point)
;; (define (sample-dimensions index dim)
;; (define (get-index-for-sample sample-num)
;; (define (mitchell-filter radius b c x point)

(require "sobol.rkt" (start-pixel sample-dimensions get-index-for-sample mitchell-filter))
(require "colors.rkt" (repeat arrR arrG arrB)) ;; accepts i

(define-generics Pixel (pixel Pixel))
(define-generics Pixels ('(pixel) Pixels))

(struct pixel (monochrome r g b )
    #:transparent
    #:methods gen:Pixel
    [(define (get-monochrome self) (monochrome self))]
    [(define (get-r self) (r self))]
    [(define (get-g self) (g self))]
    [(define (get-b self) (b self))])

(struct pixels (current) ;; additionally "variables" previously and next, however these will be represented by (- current 1) and (+ current 1). Too lazy rn to worry about circular lists! Current will end up being most likely represented by (index-ref samples2d i). 
    #:transparent
    #:methods gen:Pixels
    [(define (compare-monochrome-level self) (if (> (abs (- (maybe-ref (get-monochrome (index-ref pixel current) monochrome)) (get-monochrome (index-ref pixel (+ current 1)) monochrome))) 1) (gen-circle)))]
    [(define (compare-color self) (r self))])

(define (determine-color pixel)
  (choose-best
   (cons arrR (get-r pixel r))
   (cons arrG (get-g pixel g))
   (cons arrB (get-b pixel b))))

         
;; feel free to use the image below as an example of the sizing (512 by 512)
;;data:image/png;base64,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
(define (chosen-points)
  ;; samples points and collects their monochrome 8 bit value as a bitvector, and their respective chosen-color --> see color.rkt
  (for ([i (i < (length samples2d))])
    (let ([+ i 1]))
     (let ([pixels (mitchell-filter 0.8 0.33 0.66 (rand x -1 1) (start-pixel (sample-dimensions (get-index-for sample 16) 512)))]))
     ()))
; The following query has a solution because the second
; clause of LA2 creates two independent (??) holes.
