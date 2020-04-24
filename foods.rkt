#lang racket
(require "define-cross-phase.rkt")
(provide delicious-food? add-delicious-food!)

(define/cross-phase cross:set-member? set-member?)
(define/cross-phase cross:set-add! set-add!)

(define/cross-phase delicious-foods (mutable-set))

(define (delicious-food? food)
  (cross:set-member? delicious-foods food))

(define (add-delicious-food! new-food)
  (cross:set-add! delicious-foods new-food))