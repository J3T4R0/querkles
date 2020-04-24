#lang racket/base
(struct mapping (domain proc)
#:property prop:procedure (λ (f x) ((mapping-proc f) x)))
(struct fmapping (default hash)
#:property prop:procedure
(λ (f x) (hash-ref (fmapping-hash f) x (fmapping-default f))))
(define appx-z (make-parameter +inf.0))
(define (finitize ps)
(match-let∗ ([(mapping Ω P) ps]
[Ωn (cotake Ω (appx-z))]
[qn (apply + (map P Ωn))])
(mapping Ωn (λ (ω) (/ (P ω) qn)))))
(define ((dist X) ps)
(match-define (mapping Ω P) ps)
(fmapping 0 (for/fold ([h (hash)]) ([ω (in-list Ω)])
(hash-set h (X ω) (+ (P ω) (hash-ref h (X ω) 0))))))

(define (Geometric p)
(mapping N1 (λ (n) (∗ p (expt (- 1 p) (- n 1))))))
(define (p1-fires? n [shots 1])
(cond [(n . <= . 0) #f]
[else (not (p1-fires? (- n shots) (add1 shots)))]))

(with-model (model [winning-shot ∼ (Geometric 1/6)])
(Pr (p1-fires? winning-shot)))


(define-model half-idiot-duel
[spin? ∼ (Bernoulli 1/2)]
[winning-shot ∼ (cond [spin? (Geometric 1/6)]
[else (Uniform 1 6)])])


(define d (geometric-dist 0.4))
(plot (for/list ([i  (in-range -1 7)])
          (define i+1-ε (flprev (+ i 1.0)))
          (list (lines (list (vector i (cdf d i))
                             (vector i+1-ε (cdf d i+1-ε)))
                       #:width 2)
                (points (list (vector i (cdf d i)))
                        #:sym 'fullcircle5 #:color 1)
                (points (list (vector i+1-ε (cdf d i+1-ε)))
                        #:sym 'fullcircle5 #:color 1 #:fill-color 0)))
        #:x-min -0.5 #:x-max 6.5 #:y-min -0.05 #:y-max 1
        #:x-label "x" #:y-label "P[X ≤ x]")


;; Unfortunately we cannot define a membership test since mini-Datafun does not
;; support booleans as a semilattice type. Use Racket's (set-member? set elem)
;; function instead.

;; Intersection of two sets.
(define (intersect A B)
  (setof x
    (x <- A)
    (y <- B)
    (when (equal? x y))))

;; An equivalent, shorter definition using equality-test patterns.
;; Below, (== x) is a pattern that matches only if it receives a value equal to
;; `x`. You can find documentation on pattern-matching in Racket at
;; https://docs.racket-lang.org/reference/match.html
(define (intersect-variant A B)
  (setof x
    (x <- A)
    ((== x) <- B)))

;; The composition of binary relations R and S; relates x to z iff ∃y such that
;; xRy and ySz.
;;
;; I represent binary relations as sets of tuples, and tuples as lists of fixed
;; length. For example, a 2-tuple (x,y) is represented as '(x y).
(define (compo R S)
  (join
    ;; The pattern to match a 2-element list in Racket is (list P Q), where P
    ;; and Q are patterns for its elements.
    ((list x y)      <- R)  ;; for every tuple (x y) in R,
    ((list (== y) z) <- S)  ;; for every tuple (y z) in S, where this y equals that previous y,
    (set (list x z))))      ;; yield the tuple (x z).

 ;; ---------- Example: transitive closure ----------
;; My favorite example program: transitive closure of a relation, or determining
;; reachability (by one or more edges) in a graph.
(define (paths edges)
  ;; A path is either an edge or a composition of two paths.
  (fix (self) edges (compo self self)))

(define diamond-graph (set '(a b) '(a c) '(b d) '(c d)))
(define line-graph (set '(0 1) '(1 2) '(2 3) '(3 4)))

(define (test-paths)
  (printf "diamond-graph: ~v\n" (paths diamond-graph))
  (printf "line-graph: ~v\n" (paths line-graph)))

 ;; ---------- Example: CYK parsing ----------

;; We represent grammars as sets of production rules.
;;
;; A production rule is either:
;; - a 2-tuple (P s),   representing:   P --> s
;; - a 3-tuple (P Q R), representing:   P --> Q R
;;
;; where P, Q, R are symbols standing for nonterminals, and s is a string. This
;; is a variant of Chomsky Normal Form. It's not very convenient to write
;; grammars like this, but every context-free grammar can be put into this form.

;; Here's an example grammar, for well-balanced strings of parentheses.
;; Conceptually, it has three rules:
;;
;;     term --> ""
;;     term --> "(" term ")"
;;     term --> term term
;;
;; To put this in Chomsky normal form, we need a few auxiliary nonterminals.
(define paren-grammar
  (set '(term "")                       ;; term --> ""
       '(term LP rest) '(rest term RP)  ;; term --> "(" term ")"
       '(term term term)                ;; term --> term term
       '(LP "(") '(RP ")")))            ;; nonterminals for parentheses

;; The CYK algorithm can be viewed as a forward-chaining logic program which
;; repeatedly derives "facts" of the form (P i j), where P is a nonterminal, and
;; i and j are indices with i <= j+1. The fact (P i j) means that the substring
;; of the text from i to j inclusive can be produced by the nonterminal P.

;; In (cyk-run text grammar),
;; - `text` is the string we are trying to parse;
;; - `grammar` is the grammar (set of rules).
;; and we produce a chart of derived CYK facts.
(define (cyk-run text grammar)
  ;; `chart` is a set of derived CYK facts.
  (fix (chart)
    ;; try to derive using rules of the form (P Q R)
    ;; by concatenating already-derived facts for Q and R.
    (join ((list P Q R)           <- grammar)
          ((list (== Q) i j)      <- chart)
          ((list (== R) (== j) k) <- chart)
          (set (list P i k)))
    ;; try to derive using rules of the form (P s)
    ;; by matching `s` against every possible position in `text`.
    (join ((list P s) <- grammar)
          (define n (string-length s))
          (i <- (range (+ 1 (- (string-length text) n))))
          ;; (define _ (printf "substring ~v ~v ~v\n" text i n))
          (when (equal? s (substring text i (+ i n))))
          (set (list P i (+ i n))))))

;; Yields the set of nonterminals which can generate the given string.
(define (cyk-parse text grammar)
  (define n (string-length text))
  (define chart (cyk-run text grammar))
  (setof a ((list a 0 (== n)) <- chart)))

;; For example:
(define (balanced-parens? text)
  (set-member? (cyk-parse text paren-grammar) 'term))

;; Of course, parsing balanced parens is quite easy. But the above algorithm
;; should work for *any* CFG (as long as you put it in Chomsky normal form) -
;; even that of a full-fledged programming language.
;;
;; Performance is probably terrible, though. In theory CYK is O(n^3), a
;; respectable-but-slow worst case, but I think that to achieve that bound in
;; Datafun you'd need to index your set of facts carefully, which neither this
;; mini-Datafun nor the fuller-featured Datafun implementation we built for the
;; ICFP 2016 paper can do (yet).
;;
;; Moreover, if I recall correctly, CYK is not just O(n^3) in the worst case but
;; even in the best case. So, whether Datafun is a good language for writing
;; parsers in remains to be seen :).

; N -> [List Number Number]
; how long do searchS and searchL take 
; to look for n in (list 0 ... (- n 1))
(define (timing n)
  (local ((define long-list
            (build-list n (lambda (x) x))))
    (list
      (time (searchS n long-list))
      (time (searchL n long-list)))))

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to s, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

;;generative
(define (general P)
  (cond
    [(trivial? P) (solve P)]
    [else
     (combine-solutions
       P
       (general
         (generate P)))]))

;;structural
	
(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special (rest P)))]))
; special computes the length of its input,

; special negates each number on the given list of numbers, and

; special uppercases the given list of strings.
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))
 
; N[>= 1] N[>= 1] -> [List-of N]
; computes the divisors of l smaller or equal to k
(define (divisors k l)
  '())
 
; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l
(define (largest-common k l)
  1)