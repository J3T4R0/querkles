#lang racket/base
(require pollen/tag)
(provide (all-defined-out))
(define headline (default-tag-function 'h2))
(define items (default-tag-function 'ul))
(define item (default-tag-function 'li 'p))
(define (link url text) `(a ((href ,url)) ,text))

◊(require txexpr)
◊(require pollen/decode pollen/misc/tutorial txexpr)
◊(define (root . elements)
   (txexpr 'root empty (decode-elements elements
     #:txexpr-elements-proc decode-paragraphs
     #:string-proc (compose1 smart-quotes smart-dashes))))
(provide (all-defined-out)) ; provides `author` and `em`
(define author "Trevor Goodchild")
(define (em . elements)
  (txexpr 'extra-big empty elements))


