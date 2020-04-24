;#lang racket/base
;(require rml/data rml/individual rml-knn/classifier)	

;(define iris-data
;  (load-data-set (path->string (collection-file-path
;                                 "test/iris_training_data.csv" "rml"))
;                 'csv
;                 (list
;                  (make-feature "sepal-length" #:index 0)
;                  (make-feature "sepal-width" #:index 1)
;                  (make-feature "petal-length" #:index 2)
;                  (make-feature "petal-width" #:index 3)
;                  (make-classifier "classification" #:index 4))))
;
;(define an-iris
;  (make-individual #:data-set iris-data
;                   "sepal-length" 6.3
;                     "sepal-width" 2.5
;                     "petal-length" 4.9
;                     "petal-width" 1.5
;                     "classification" "Iris-versicolor"))
;(define classify (make-knn-classifier 5))
;(classify iris-data default-partition an-iris)

;;;; Different Implementation ;;;;;
;(define (iris-distance-euclidean from to)
;  (let (
;        [sld (expt (- (iris-sepal-length from) (iris-sepal-length to)) 2)]
;        [swd (expt (- (iris-sepal-width from) (iris-sepal-width to)) 2)]
;        [pld (expt (- (iris-petal-length from) (iris-petal-length to)) 2)]
;        [pwd (expt (- (iris-petal-width from) (iris-petal-width to)) 2)]
;        )
 
;    (cons (sqrt (+ sld swd pld pwd)) (iris-classification to))))
 
;(define classify-me (iris 6.3 2.5 4.9 1.5 "Iris-versicolor"))
;(map (curry iris-distance-euclidean classify-me) training-data)
;(define (iris-distance-pair-< i1 i2)
 ; (< (car i1) (car i2)))
 
;(take 
;  (sort 
;    (map (curry iris-distance-euclidean classify-me) training-data) 
;    iris-distance-pair-<) 
;  5)

;(define (unique-classes distance-pairs)
 ; (list->set (map cdr distance-pairs)))
 
;(define (pair-cdr-is? x pair)
;  (equal? (cdr pair) x))

;(define (count-all-classes distance-pairs)
;  (define (_count-all classes accum)
;    (if (set-empty? classes)
;      accum
      ;(_count-all
     ;   (set-rest classes)
    ;    (cons
   ;       (cons (count (curry pair-cdr-is? (set-first classes)) distance-pairs) (set-first classes))
  ;        accum)
 ;       )
      )
;    )
 
;  (_count-all (unique-classes distance-pairs) (list))
;)
 
;(count-all-classes k-neighbors)