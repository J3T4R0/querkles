#lang racket
(require syntax/parse/define "foods.rkt" (for-syntax "foods.rkt"))

(add-delicious-food! "pineapple")
(add-delicious-food! "sushi")
(add-delicious-food! "cheesecake")

(define-simple-macro (add-food-combinations! [fst:string ...]
                                             [snd:string ...])
  #:do [(for* ([fst-str (in-list (syntax->datum #'[fst ...]))]
               [snd-str (in-list (syntax->datum #'[snd ...]))])
          (add-delicious-food! (string-append fst-str " " snd-str)))]
  (void))

; should add “fried chicken,” “roasted chicken”, “fried potato,” and “roasted potato”
(add-food-combinations! ["fried" "roasted"] ["chicken" "potato"])

(command-line
  #:args [food-to-check]
  (if (delicious-food? food-to-check)
      (printf "~a is a delicious food.\n" food-to-check)
      (printf "~a is not delicious.\n" food-to-check)))