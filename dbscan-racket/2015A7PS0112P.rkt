#lang racket
(provide (all-defined-out))

#| DEFINING HELPER FUNCTIONS
==============================

  These functions are used for various tasks and have been used throughout the code.

    - (helper-getRawLinesInFile file) : yields the list of lines in file
    - (helper-getStringList str) : splits the string str using space
    - (helper-convertStrListToNumList ls) : converts list of strings into list of numbers

    - (helper-getNumbersFromFile file) : combines the power of the above functions to read the numbers from the file
|#

(define (helper-getRawLinesInFile file) ; yields list of strings corresponding to each line
  (define line (read-line file))
  (if (eof-object? line)
      null
      (cons line (helper-getRawLinesInFile file))
  )
)

(define (helper-getStringList str) ; splits string using space
  (string-split str)
)

(define (helper-convertStrListToNumList ls) ; converts list of strings into list of numbers
  (map string->number ls)
)

(define (helper-getNumbersFromFile in)
  (map helper-convertStrListToNumList (map helper-getStringList (helper-getRawLinesInFile in)))
)

#| ======================================================================================================== |#
#| READING FILE
================|#

(define args (current-command-line-arguments))
(define infile (vector-ref args 0))
;(define infile "t0.in")
(define fin (open-input-file infile))

(define testCase (helper-getNumbersFromFile fin))

(define header (car testCase))
(define dataPoints (cdr testCase))

(define N (list-ref header 0))
(define D (list-ref header 1))
(define K (list-ref header 2))
(define e (list-ref header 3))
(define MinPts (list-ref header 4))

#| ======================================================================================================== |#
#| STEP 1
==========|#

(define (constructStep1 N counter dataPoints)
  (cond
    ((> counter N) '())
    (else (cons (cons counter (list (car dataPoints))) (constructStep1 N (+ counter 1) (cdr dataPoints))))
  )
)

(define step1 (constructStep1 N 1 dataPoints))
;step1

#| ======================================================================================================== |#
#| STEP 2
==========|#

(define (euclideanDistance p1 p2 D result)

  ; ARGS DESC
  ; p1 = '(1 2 3 4)
  ; p2 = '(5 6 7 8)
  ; D = 4
  ; result = 0
  
  (cond
    ((= D 0) (sqrt result))
    (else (euclideanDistance (cdr p1) (cdr p2) (- D 1) (+ result (expt (- (car p1) (car p2)) 2))))
  )
)

(define (similarityMatrixRowGenerator dataPoints N D i j)

  ; ARGS DESC
  ; dataPoints = from step above
  ; N = from file
  ; D = from file
  ; i = (point i)
  ; j = (point j)
  
  (cond
    ((> j N) '())
    ((= j i) (cons (append (cons j (list +inf.0))) (similarityMatrixRowGenerator dataPoints N D i (+ j 1))))
    (else
     (define Pj (list-ref dataPoints (- j 1)))
     (define Pi (list-ref dataPoints (- i 1)))
     (cons (cons j (list (euclideanDistance Pi Pj D 0))) (similarityMatrixRowGenerator dataPoints N D i (+ j 1)))
    )
  )
)

(define (constructStep2 N D iter)

  ; ARGS DESC
  ; N = from file
  ; D = from file
  ; iter = 1 (iteration number)
  
  (cond
    ((> iter N) '())
    (else (cons (similarityMatrixRowGenerator dataPoints N D iter 1) (constructStep2 N D (+ iter 1))))
  )
)

(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define step2 (modify_precision (constructStep2 N D 1)))
; step2

#| ======================================================================================================== |#
#| STEP 3
==========|#

(define (midPos ls)
  (floor (/ (length ls) 2))
)

(define (get-head  ls l) "undefined")
(define (get-tail ls l) "undefined")

(define (leftList ls len mp)
  (cond
    ((null? ls) '())
    ((> mp len) '())
    ((> mp 0) (cons (car ls) (leftList (cdr ls) (- len 1) (- mp 1))))
    (else '())
  )
)

(define (rightList ls len mp)
  (cond
    ((null? ls) '())
    ((> mp len) '())
    ((> mp 0) (rightList (cdr ls) (- len 1) (- mp 1)))
    (else ls)
  )
)

#|
  Defining a dictionary like data structure. Here, I call the list of the form
  ( (key1 data1) (key2 data2) ...)
  as a dictionary. This will help 
|#

(define (dictMake keyList dataList)
  (if (= (length keyList) (length dataList))
      (if (null? keyList)
          '()
          (cons (list (car keyList) (car dataList)) (dictMake (cdr keyList) (cdr dataList)))
      )
      '()
  )
)

(define (dictKey ls)
  (car ls)
)

(define (dictData ls)
  (car (cdr ls))
)

(define (dictDataList ls)
  (map (dictData ls))
)

(define (dictKeyList ls)
  (map (dictKey ls))
)

(define (dictGet ls key)
  (cond
    ((null? ls) null)
    ((equal? (dictKey (car ls)) key) (dictData (car ls)))
    (else (dictGet (cdr ls) key))
  )
)

(define (dict<=d item1 item2)
  (<= (dictData item1) (dictData item2))
)

(define (dict>=d item1 item2)
  (>= (dictData item1) (dictData item2))
)

(define (dict<=k item1 item2)
  (<= (dictKey item1) (dictData item2))
)

(define (dict>=k item1 item2)
  (>= (dictKey item1) (dictData item2))
)

#|
  Merge Sort - to sort the dictionary based on values instead of keys
|#

(define (mergeList ls1 ls2 op)
  (cond
    ((null? ls1) ls2)
    ((null? ls2) ls1)
    (else
      (cond
        ((op (dictKey (car ls1)) (dictKey (car ls2))) (cons (car ls1) (mergeList (cdr ls1) ls2 op)))
        ((op (dictKey (car ls2)) (dictKey (car ls1))) (cons (car ls2) (mergeList ls1 (cdr ls2) op)))
      )
    )
  )
)

(define (mergeSort ls)
  (cond
    ((null? ls) '())
    ((= (length ls) 1) ls)
    (else
      (define Len (length ls))
      (define mp (midPos ls))
      
      (define ll (mergeSort (leftList ls Len mp))) 

      (define rl (mergeSort (rightList ls Len mp)))

      (define ml (mergeList ll rl <=))
      ml
    )
  )
)

(define (getPoint pts i)
  (if (null? pts)
      '() 
      (cond
        ((= i 1) (dictData (car pts)))
        ((> i 1) (getPoint (cdr pts) (- i 1)))
      )
  )
)

(define (KNN step2Elem N K)
  (define data (map dictData step2Elem))
  (define keys (map dictKey step2Elem))

  (define inverse-dict (dictMake data keys))

  (define knnList (leftList (map dictData (mergeSort inverse-dict)) N K))
  (leftList (map dictData (mergeSort (dictMake knnList knnList))) N K)
  ;(leftList (mergeSort (dictMake knnList knnList)) N K)
)

(define (constructStep3 step2 N K)
  (cond
    ((null? step2) '())
    (else (cons (KNN (car step2) N K) (constructStep3 (cdr step2) N K)))
  )
)

(define step3 (constructStep3 step2 N K))
;step3

#| ======================================================================================================== |#
#| STEP 4
==========|#

(define (isInList i ls)
  (cond
    ((null? ls) #f)
    ((= i (car ls)) #t)
    (else (isInList i (cdr ls)))
  )
)

(define (binSearch ls i)
  (define mp (midPos ls))
  (define len (length ls))
  (cond
    ((null? ls) #f)
    ((equal? i (list-ref ls mp)) #t)
    ((> i (list-ref ls mp)) (binSearch (rightList ls len (+ mp 1)) i))
    ((< i (list-ref ls mp)) (binSearch (leftList ls len mp) i))
  )
)

(define (commonItemsCount ls1 ls2 C) ; assumes sorted lists ls1 and ls2 to apply binary searach
  (if (null? ls1)
      #|then|# C
      #|else|# (if (binSearch ls2 (car ls1))
                    #|then|# (commonItemsCount (cdr ls1) ls2 (+ C 1))
                    #|else|# (commonItemsCount (cdr ls1) ls2 C)
               )
  )
)

(define (snList step3List ithNN i K pos)
  #|
     pos goes from 0 to K-1
  |#
  (cond
    ((>= pos K) '())
    (else
     (define jthNN (list-ref step3List (- (list-ref ithNN pos) 1)))
     (if (binSearch jthNN i)
         (cons (cons (list-ref ithNN pos) (list (commonItemsCount ithNN jthNN 0))) (snList step3List ithNN i K (+ pos 1)))
         (snList step3List ithNN i K (+ pos 1))
     )
    )
  )
)

(define (compForSNGsort item1 item2)
  (cond
    ((= (dictData item1) (dictData item2)) (dict<=k item1 item2))
    (else (dict>=d item1 item2))
  )
)

(define (constructStep4 step3List i N)
  (cond
    ((> i N) '())
    (else (cons (sort (snList step3List (list-ref step3List (- i 1)) i K 0) compForSNGsort) (constructStep4 step3List (+ i 1) N)))
  )
)

(define step4 (constructStep4 step3 1 N))
;step4

#| ======================================================================================================== |#
#| STEP 5
==========|#

(define step5 (map length step4))
;step5

#| ======================================================================================================== |#
#| STEP 6
==========|#

(define (corePointList densities i)
  (if (null? densities) '()
      (cond
        ((>= (car densities) MinPts) (cons i (corePointList (cdr densities) (+ i 1))))
        (else (corePointList (cdr densities) (+ i 1)))
      )
  )
)

(define step6 (corePointList step5 1))
;step4
;step6

(define CORE_POINTS step6)
(define SHARED_NEIGHBOURS step4)

(define (isCorePoint p)
  (binSearch CORE_POINTS p)
)

#| ======================================================================================================== |#
#| STEP 7
==========|#

(define (removeElem ls e)
  (cond
    ((null? ls) '())
    ((equal? (car ls) e) (cdr ls))
    (else (cons (car ls) (removeElem (cdr ls) e)))
  )
)

(define (subtractLists ls1 ls2)
  (cond
    ((null? ls2) ls1)
    ((null? ls1) '())
    (else (subtractLists (removeElem ls1 (car ls2)) (cdr ls2)))
  )
)

(define (linSearch ls e)
  (cond
    ((null? ls) #f)
    ((equal? e (car ls)) #t)
    (else (linSearch (cdr ls) e))
  )
)

(define (explore start_point core_points sharedNeighbours nb)
  (cond
    ((null? nb) '())
    (else
     (define p (dictKey (car nb)))
     (cond
       
       ((linSearch core_points p) (cons p (explore p (removeElem core_points p) sharedNeighbours (list-ref sharedNeighbours (- p 1)) )))
       (else (explore start_point core_points sharedNeighbours (cdr nb)))
     )
    )
  )
)
  
(define (constructStep7 corePoints sharedNeighbours cluster_id)
  (cond
    ((null? corePoints) '())
    (else
      (define sp (car corePoints))
      ;(display sp)
      (define cluster (sort (explore sp (removeElem corePoints sp) sharedNeighbours (list-ref sharedNeighbours (- sp 1))) <))
      ;(display cluster)
      (define newCorePoints (subtractLists (removeElem corePoints sp) cluster))
      (if (null? cluster)
          (constructStep7 newCorePoints sharedNeighbours cluster_id)
          (cons (cons cluster_id (list cluster)) (constructStep7 newCorePoints sharedNeighbours (+ cluster_id 1)))
      )
    )
  )
)

(define step7 (constructStep7 CORE_POINTS SHARED_NEIGHBOURS 1))

#| ======================================================================================================== |#
#| STEP 8
==========|#

(define (getPointList n)
  (if (= n 0) '()
      (cons n (getPointList (- n 1)))
      )
 )

(define (getNoisePoints points coreClusters)
  (cond
    ((null? coreClusters) points)
    (else (getNoisePoints (subtractLists points (car (dictData coreClusters))) (cdr coreClusters)))
  )
)

;(define step8 (getNoisePoints (getPointList N) step7))
(define step8 0)
(define step9 0)
(define step10 0)
