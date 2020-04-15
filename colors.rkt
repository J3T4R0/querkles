#lang racket
(require (planet neil/csv:1:=7) net/url)
;#!racket/base

;(define-values (pair-for-each map filter-map filter zip unzip)
;  (let ((%MAP-PASS (list 'MAP-PASS))
;        (%MAP-END (list 'MAP-END)))
;
;    ;; pair-for-each-1 applies proc to every cons
;    ;; in order until proc returns %MAP-END
;    ;; when proc evaluates to %MAP-PASS the result is skipped
;    (define (pair-for-each-1 proc lst (next cdr))
;      (let loop ((lst lst))
;        (let ((res (proc lst)))
;          (cond ((eq? res %MAP-END) '())
;                ((eq? res %MAP-PASS) (loop (next lst)))
;                (else (cons res
;                            (loop (next lst))))))))
;
;    ;; Transform a typical map procedure to include
;    ;; a %MAP-END when the list argument is eq? a certain value
;    (define (stop-at value proc)
;      (lambda (lst)
;        (if (eq? value lst)
;            %MAP-END
;            (proc lst))))
;
;    ;; Takes a lists of lists and returns a
;    ;; new list with the cdrs
;    (define (cdrs lsts)        
;      (pair-for-each-1 (stop-at '() cdar) lsts))
;
    ;; Takes a list of lists and returns a
    ;; new list with the cars except if one of
    ;; the sublists are nil in which the result is also nil
;    (define (cars lsts)
;      (call/cc (lambda (exit)
;                 (pair-for-each-1 (stop-at '() 
;                                           (lambda (x)
;                                             (let ((x (car x)))
;                                               (if (null? x)
;                                                   (exit '())
;                                                   (car x))))) 
;                                  lsts))))

    ;; Takes a list of lists and returns #t if any are null
;    (define (any-null? lsts)
;      (if (null? lsts)
;          #f
;          (or (null? (car lsts))
;              (any-null? (cdr lsts)))))
;
;    ;; Return value definitions starts here
;
;    ;; pair-for-each is called maplist in CL
;    (define (pair-for-each proc lst . lsts)
;      (if (null? lsts)
;          (pair-for-each-1 (stop-at '() (lambda (x) (proc x))) lst)
;          (pair-for-each-1 (lambda (args)
;                             (if (any-null? args)
;                                 %MAP-END
;                                 (apply proc args))) 
;                           (cons lst lsts) 
;                           cdrs)))


    ;; Multi arity map
;    (define (map f lst . lsts)
;      (if (null? lsts)
;          (pair-for-each-1 (stop-at '() (lambda (x) (f (car x)))) lst)
;          (pair-for-each-1 (lambda (x)
;                             (let ((args (cars x)))
;                               (if (null? args)
;                                   %MAP-END
;                                   (apply f args)))) 
;                           (cons lst lsts) 
;                           cdrs)))

    ;; filter-map is like map except it skips false values
;    (define (filter-map proc . lsts)
;      (apply map (lambda x
;                   (or (apply proc x) %MAP-PASS)))
;             lsts)

    ;; filter only takes one list and instead of the result it
    ;; takes the original argument as value (which may be #f)
;    (define (filter predicate? lst)
;      (pair-for-each-1 (stop-at '() 
;                                (lambda (x)
;                                  (let ((x (car x)))
;                                    (if (predicate? x)
;                                        x
;                                        %MAP-PASS))))
;                       lst))

    ;; zip (zip '(1 2 3) '(a b c)) ; ==> ((1 a) (2 b) (3 c))
;    (define (zip lst . lsts)
;      (apply map list (cons lst lsts)))

    ;; unzip does the same except it takes a list of lists as argument
;    (define (unzip lsts)
;      (apply map list lsts))

    ;; return procedures
;    (values pair-for-each map filter-map filter zip unzip)))

;; API SPEC ;;
;; functions: 
;; 
;;
;;
;;
;;
;;
;;
;;
;; sort : list-of-numbers -> list-of-numbers 
;; to create a list of numbers with the same 
;; numbers 
;; as alon sorted in descending order 
;(define (sort alon) 
;  (cond 
;    [(empty? alon) empty] 
;    [(cons? alon) (insert (first alon) 
;                        (sort (rest alon)))]))

;; insert : number sorted-number-list -> number-list 
;; to create a list of numbers from n and alon that is 
;; sorted in ascending order; alon is sorted 
(define (insert n alon) 
  (cond 
    [(empty? alon) (cons n empty)] 
    [else (cond 
            [(< n (first alon)) (cons n alon)] 
            [else (cons (first alon) 
    (insert n (rest alon)))])])) 

(define (partition left? ls)
  (match ls
    ('() (cons '() '()))
    ((cons hd tl)
     (match
         (partition left? tl)
       ((cons l r)
        (if (left? hd)
            (cons (cons hd l) r)
            (cons l (cons hd r))))))))


(define (quicksort compare ls)
  (match ls
    ('() '())
    ((cons hd tl)
     (match (partition (lambda (x) (compare x hd)) tl)
       ((cons l r)
        (append
         (quicksort compare l)
         (list hd)
         (quicksort compare r)))))))

;; example: (quicksort < '(3 4 5 R 1 7 6))



  
;(let* ([ph (make-placeholder #f)]
;         [x (cons 1 ph)])
;    (placeholder-set! ph x)
;    (make-reader-graph x))
;#0='(1 . #0#)

;(array-map (λ: ([n : Natural]) (* 2 n)) arr)
;(array+ arr arr)

;RP stands for R plus, or the value above R
;RM stands for R minus, or the value below R
(define (choose-best arrR arrG arrB)
  (define R 0)
  (define G 0)
  (define B 0)

  (define arrR (quicksort < #[(index-ref colors #[i, 0]) ,R]))
  (define arrG (quicksort < #[(index-ref poss-g #[i, 1]) ,G]))
  (define arrB (quicksort < #[(index-ref poss-b #[i, 2]) ,B]))
  (define colors #[arrR arrG arrB])
  (let ([RP (car (cdar (take-right arrR (index-of (arrR) R))))]))
  (let ([GP (car (cdar (take-right arrG (index-of (arrG) G))))]))
  (let ([BP (car (cdar (take-right arrB (index-of (arrB) B))))]))

  (let ([RM (list-tail (drop-right arrR (index-of (arrR) R)) 1)]))
  (let ([GM (list-tail (drop-right arrG (index-of (arrG) G)) 1)]))
  (let ([BM (list-tail (drop-right arrB (index-of (arrB) B)) 1)]))
  '((if-let (> (abs (- RP R)) (abs (- RM R))) RP RM)
  (if-let (> (abs (- GP G)) (abs (- GM G))) GP GM)
  (if-let (> (abs (- BP B)) (abs (- BM G))) BP BM)))

(define galapogos-url
  (string->url
    "https://github.com/J3T4R0/querkles/colors.csv"))

(define make-galapagos-csv-reader
  (make-csv-reader-maker
   '((separator-chars                      #\,)
     (strip-leading-whitespace?          . #t)
     (strip-trailing-whitespace?         . #t))))

(define (all-rows url make-reader)
  (define next-row (make-reader (get-pure-port url)))
  (define (loop)
    (define row (next-row))
    (if (empty? row)
        '()
        (cons row (loop)))))


(all-rows galapogos-url make-galapagos-csv-reader)

(for/fold ([prev #f]
           [counter 1])
           ([parsed-red arrR]
            #:when (not (equal? parsed-red R)))
  (printf "~a. ~a\n" counter chapter)
  (values parsed-red
          (add1 counter)))


(for*/list ([(k v) #hash(("match-red" . '(poss-r, 0, 0)) ("match-green" . '(0, poss-g, 0)) ("match-blue" . '(poss-b, 0, 0)))])
            [(i) (in-range v)]
  k)

(define (repeat i)
(let ([seq (in-list pixels)]))
(for ([i (in-range colors)])
   (for ([elem seq])
     (choose-best))))


