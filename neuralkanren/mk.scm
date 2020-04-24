;; Contents:
;; 0. Convenient shorthands
;; 1. Data model for logic programming with transparent constraint trees
;; 2. Constraint tree visualization
;; 3. Standard biased interleaving search interface
;; 4. Standard miniKanren EDSL (Embedded Domain Specific Language) definitions
;; 5. Interface for externally-driven search
;; 6. Performance testing interface for biased interleaving search with pruning

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0. Convenient shorthands
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax let*/and
  (syntax-rules ()
    ((_ () rest ...) (and rest ...))
    ((_ ((name expr) ne* ...) rest ...)
     (let ((name expr))
       (and name (let*/and (ne* ...) rest ...))))))

(define-syntax defrecord
  (syntax-rules ()
    ((_ name name?)
     (begin
       (define name (vector 'name))
       (define (name? datum) (eq? name datum))))
    ((_ name name? (field set-field) ...)
     (begin
       (define (name field ...) (vector 'name field ...))
       (define (name? datum)
         (and (vector? datum) (eq? 'name (vector-ref datum 0))))
       (let ()
         (define (range-assoc start xs)
           (let loop ((xs xs) (idx start))
             (if (null? xs)
               '()
               (cons (cons (car xs) idx) (loop (cdr xs) (+ idx 1))))))
         (define (define-field-getter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum) (vector-ref datum ,idx))))
         (define (define-field-setter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum value)
                    (let ((new (vector-copy datum)))
                      (vector-set! new ,idx value)
                      new))))
         (let ((fns (range-assoc 1 '(field ...))))
           (begin (define-field-getter 'field fns) ...))
         (let ((set-fns (range-assoc 1 '(set-field ...))))
           (begin (define-field-setter 'set-field set-fns) ...)))))
    ((_ name name? field ...)
     (begin
       (define (name field ...) (vector 'name field ...))
       (define (name? datum)
         (and (vector? datum) (eq? 'name (vector-ref datum 0))))
       (let ()
         (define (range-assoc start xs)
           (let loop ((xs xs) (idx start))
             (if (null? xs)
               '()
               (cons (cons (car xs) idx) (loop (cdr xs) (+ idx 1))))))
         (define (define-field-getter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum) (vector-ref datum ,idx))))
         (let ((fns (range-assoc 1 '(field ...))))
           (begin (define-field-getter 'field fns) ...)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Data model for logic programming with transparent constraint trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Read section "3. Standard biased interleaving search interface" to see how
;; a typical miniKanren search implementation maps onto this data model.

;; The full constraint tree grammar consists of:
;; * conjunctions (logical AND) with two children
(defrecord conj conj? conj-c1 conj-c2)
;; * disjunctions (logical OR) with two children
(defrecord disj disj? disj-c1 disj-c2)
;; * recursive constraints that are currently suspended
(defrecord zzz zzz? zzz-metadata zzz-wake)
;; * subtrees that have not yet propagated equality info (stored in a state)
(defrecord pause pause? pause-state pause-goal)
;; * equalities between two terms
(defrecord == ==? ==-t1 ==-t2)
;; The interaction system currently only presents constraint trees that are in
;; disjunctive normal form (DNF), and that have propagated all equality
;; information, meaning no `pause` or `==` nodes remain.

;; Logic variables
(defrecord var var? var-index)
(define var/fresh
  (let ((index -1))
    (lambda ()
      (set! index (+ 1 index))
      (var index))))
;(define var=? eq?)
(define (var=? t1 t2)
  (and (var? t1) (var? t2) (eqv? (var-index t1) (var-index t2))))
(define (var<? v1 v2) (< (var-index v1) (var-index v2)))
(define var-initial (var/fresh))

;; States describing constraint information.  Currently, only equality
;; constraints are supported, and stored as an association list of
;; variable bindings.
(define store-empty '())
(define (store-empty? store) (null? store))
(define (store-ref store key . default)
  (let ((binding (assoc key store)))
    (if binding
      (cdr binding)
      (if (null? default)
        (error 'store-ref (format "missing key ~s in ~s" key store))
        (car default)))))
(define (store-set store key value) `((,key . ,value) . ,store))

(define (vattrs-get vs vr) (store-ref vs vr vr))
(define (vattrs-set vs vr value) (store-set vs vr value))
(define (walk-vs vs tm)
  (if (var? tm)
    (let ((va (vattrs-get vs tm)))
      (if (var=? tm va)
        tm
        (walk-vs vs va)))
    tm))

(defrecord state state? (state-vs set-state-vs))
(define state-empty (state store-empty))
(define (state-empty? st) (store-empty? (state-vs st)))
(define (state-var-get st vr) (vattrs-get (state-vs st) vr))
(define (state-var-set st vr value)
  (set-state-vs st (vattrs-set (state-vs st) vr value)))

;; Unification (for implementing equality constraints)
(define (state-var-== st vr value)
  (let*/and ((st (not-occurs? st vr value)))
    (state-var-set st vr value)))
(define (state-var-==-var st v1 v2)
  (if (var<? v1 v2)  ;; Pointing new to old may yield flatter substitutions.
    (state-var-set st v2 v1)
    (state-var-set st v1 v2)))

(define (walk st tm) (walk-vs (state-vs st) tm))

(define (not-occurs? st vr tm)
  (if (pair? tm)
    (let*/and ((st (not-occurs? st vr (walk st (car tm)))))
      (not-occurs? st vr (walk st (cdr tm))))
    (and (not (var=? vr tm)) st)))

(define (unify st t1 t2)
  (and st (let ((t1 (walk st t1)) (t2 (walk st t2)))
            (cond
              ((eqv? t1 t2) st)
              ((var? t1)
               (if (var? t2)
                 (state-var-==-var st t1 t2)
                 (state-var-== st t1 t2)))
              ((var? t2) (state-var-== st t2 t1))
              ((and (pair? t1) (pair? t2))
               (let*/and ((st (unify st (car t1) (car t2))))
                 (unify st (cdr t1) (cdr t2))))
              (else #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Constraint tree visualization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (store-sort vs)
  (define (odds xs)
    (cond ((null? xs) '())
          ((null? (cdr xs)) (list (car xs)))
          (else (cons (car xs) (odds (cddr xs))))))
  (define (merge xs ys)
    (cond ((null? xs) ys)
          ((null? ys) xs)
          ((<= (var-index (caar xs)) (var-index (caar ys)))
           (cons (car xs) (merge (cdr xs) ys)))
          (else (cons (car ys) (merge xs (cdr ys))))))
  (cond ((null? vs) '())
        ((null? (cdr vs)) vs)
        (else (merge (store-sort (odds vs))
                     (store-sort (odds (cdr vs)))))))

;; A state describes equality constraints, associating variables with values.
;; Since we currently flatten constraint trees into disjunctive normal form,
;; and propagate equality information, only the binding for var-initial is
;; interesting.  If we omit this flattening, bindings for other variables will
;; often be necessary to fully specify the constraint tree.  If we inccorporate
;; primitive constraints beyond equality (i.e. disequality, types, etc.), we
;; will have to provide these as well.
(define (state-pretty st)
  ;; Minimal state mode:
  `(state ((== ,(reify-var (var-index var-initial))
               ,(reify #f st var-initial))))
  ;; Full state mode:
  ;`(state ,(map (lambda (kv) `(== ,(reify-var (var-index (car kv)))
                                  ;,(reify #f st (cdr kv))))
                ;(store-sort (state-vs st))))
  )
(define (goal-pretty goal)
  (cond
    ((conj? goal)
     `(conj ,(goal-pretty (conj-c1 goal)) ,(goal-pretty (conj-c2 goal))))
    ((disj? goal) `(disj ,(goal-pretty (disj-c1 goal)) ,(goal-pretty (disj-c2 goal))))
    ((zzz? goal) (zzz-metadata goal))
    ((==? goal) `(== ,(==-t1 goal) ,(==-t2 goal)))))
(define (stream-find-state ss)
  (cond ((conj? ss) (stream-find-state (conj-c1 ss)))
        ((pause? ss) (pause-state ss))
        (else state-empty)))
(define (stream-pretty ss)
  (define (pretty ss)
    (cond
      ((conj? ss) `(conj ,(pretty (conj-c1 ss))
                         ,(reify #f (stream-find-state (conj-c1 ss))
                                 (goal-pretty (conj-c2 ss)))))
      ((disj? ss) `(disj ,(pretty (disj-c1 ss)) ,(pretty (disj-c2 ss))))
      ((pause? ss)
       `(pause ,(state-pretty (pause-state ss))
               ,(reify #f (pause-state ss) (goal-pretty (pause-goal ss)))))
      (else ss)))
  (let loop ((ss ss) (states '()))
    (cond
      ((state? ss) (loop #f (cons ss states)))
      ((pair? ss) (loop (cdr ss) (cons (car ss) states)))
      (else (list (map reify-initial (reverse states)) (pretty ss))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Standard biased interleaving search interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bind ss goal)
  (cond
    ((not ss) #f)
    ((state? ss) (start ss goal))
    ((pair? ss) (disj (pause (car ss) goal) (conj (cdr ss) goal)))
    (else (conj ss goal))))
(define (mplus s1 s2)
  (cond
    ((not s1) s2)
    ((state? s1) (cons s1 s2))
    ((pair? s1) (cons (car s1) (disj s2 (cdr s1))))
    (else (disj s2 s1))))

(define (start st goal)
  (cond
    ((conj? goal) (bind (start st (conj-c1 goal)) (conj-c2 goal)))
    ((disj? goal) (disj (pause st (disj-c1 goal)) (pause st (disj-c2 goal))))
    ((zzz? goal) (start st ((zzz-wake goal))))
    ((==? goal) (unify st (==-t1 goal) (==-t2 goal)))
    (else (error 'start (format "invalid goal to start: ~s" goal)))))

(define (continue ss)
  (cond
    ((conj? ss) (bind (continue (conj-c1 ss)) (conj-c2 ss)))
    ((disj? ss) (mplus (continue (disj-c1 ss)) (disj-c2 ss)))
    ((pause? ss) (start (pause-state ss) (pause-goal ss)))
    ((not ss) #f)
    ((state? ss) (cons ss #f))
    (else (error 'start (format "invalid stream to continue: ~s" ss)))))

(define (step n ss)
  (cond
    ((= 0 n) ss)
    ((not ss) #f)
    ((pair? ss) (cons (car ss) (step n (cdr ss))))
    (else (step (- n 1) (continue ss)))))

(define (stream-next ps)
  (define ss (continue ps))
  (cond
    ((not ss) '())
    ((state? ss) (cons ss #f))
    ((pair? ss) ss)
    (else (stream-next ss))))
(define (stream-take n ps)
  (if (and n (= 0 n))
    '()
    (let ((ss (stream-next ps)))
      (if (pair? ss)
        (cons (car ss) (stream-take (and n (- n 1)) (cdr ss)))
        '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Standard miniKanren EDSL definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) body ...)
     (define (name param ...)
       (zzz `(name ,param ...) (lambda () body ...))))))

(define succeed (== #t #t))
(define fail (== #f #t))

(define-syntax conj*
  (syntax-rules ()
    ((_) succeed)
    ((_ g) g)
    ((_ gs ... g-final) (conj (conj* gs ...) g-final))))
(define-syntax disj*
  (syntax-rules ()
    ((_) fail)
    ((_ g) g)
    ((_ g0 gs ...) (disj g0 (disj* gs ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (vr ...) g0 gs ...) (let ((vr (var/fresh)) ...) (conj* g0 gs ...)))))
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 gs ...)) (conj* g0 gs ...))
    ((_ c0 c1 cs ...) (disj (conde c0) (conde c1 cs ...)))))

(define (run-goal n st goal) (stream-take n (pause st goal)))

(define (walk* st tm)
  (let ((tm (walk st tm)))
    (if (pair? tm)
      `(,(walk* st (car tm)) . ,(walk* st (cdr tm)))
      tm)))

(define (reify-var idx)
  (string->symbol (string-append "_." (number->string idx))))
(define (reify index st tm)
  (let loop
    ((rvs store-empty) (index index) (tm tm) (k (lambda (rvs i tm) tm)))
    (let ((tm (walk st tm)))
      (cond
        ((var? tm)
         (let* ((idx (store-ref rvs tm (or index (var-index tm))))
                (n (reify-var idx)))
           (if (eqv? index idx)
             (k (store-set rvs tm index) (+ 1 index) n)
             (k rvs index n))))
        ((pair? tm) (loop rvs index (car tm)
                          (lambda (r i a)
                            (loop r i (cdr tm)
                                  (lambda (r i d) (k r i `(,a . ,d)))))))
        (else (k rvs index tm))))))
(define (reify-initial st) (reify 0 st var-initial))

(define-syntax query
  (syntax-rules ()
    ((_ (vr ...) g0 gs ...)
     (let ((goal (fresh (vr ...) (== (list vr ...) var-initial) g0 gs ...)))
       (pause state-empty goal)))))
(define-syntax run
  (syntax-rules ()
    ((_ n body ...) (map reify-initial (stream-take n (query body ...))))))
(define-syntax run*
  (syntax-rules ()
    ((_ body ...) (run #f body ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Interface for externally-driven search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prune-depth 50)

(define (state*->disj ss)
  (define (success st) (pause st succeed))
  (cond ((pair? ss) (disj (success (car ss)) (state*->disj (cdr ss))))
        ((state? ss) (success ss))
        (else ss)))

(define (trivial-success? ss)
  (and (==? ss) (state-empty? (unify state-empty (==-t1 ss) (==-t2 ss)))))

(define (wake-path path ss)
  (cond
    ((conj? ss) (wake-bind (wake-path path (conj-c1 ss)) (conj-c2 ss)))
    ((disj? ss)
     (cond ((null? path) (error 'wake-path (format "path is too short ~s" ss)))
           ((car path) (wake-mplus (wake-path (cdr path) (disj-c1 ss))
                                   (disj-c2 ss)))
           (else (wake-mplus (disj-c1 ss)
                             (wake-path (cdr path) (disj-c2 ss))))))
    ((pause? ss) (wake-path-goal path (pause-state ss) (pause-goal ss)))
    (else (error 'wake-path (format "cannot wake ~s ~s" path ss)))))
(define (wake-path-goal path st g)
  (cond
    ((conj? g) (wake-bind (wake-path-goal path st (conj-c1 g)) (conj-c2 g)))
    ((disj? g)
     (cond
       ((null? path) (error 'wake-path-goal (format "path is too short ~s" g)))
       ((car path) (wake-mplus (wake-path-goal (cdr path) st (disj-c1 g))
                               (pause st (disj-c2 g))))
       (else (wake-mplus (pause st (disj-c1 g))
                         (wake-path-goal (cdr path) st (disj-c2 g))))))
    ((zzz? g)
     (if (pair? path)
       (error 'wake-path-goal (format "path is too long ~s ~s" path g))
       (prune-goal prune-depth st ((zzz-wake g)))))
    ((==? g)
     (if (pair? path)
       (error 'wake-path-goal (format "path is too long ~s ~s" path g))
       (unify st (==-t1 g) (==-t2 g))))
    (else (error 'wake-path-goal (format "cannot wake ~s ~s" path g)))))
(define (wake-mplus c1 c2)
  (cond ((not c1) c2)
        ((state? c1) (cons c1 c2))
        ((pair? c1) (cons (car c1) (wake-mplus (cdr c1) c2)))
        ((not c2) c1)
        ((state? c2) (cons c2 c1))
        ((pair? c2) (cons (car c2) (wake-mplus c1 (cdr c2))))
        (else (disj c1 c2))))
(define (wake-bind ss goal)
  (cond ((not ss) #f)
        ((state? ss) (prune-goal prune-depth ss goal))
        ((pair? ss) (wake-bind (state*->disj ss) goal))
        ((disj? ss) (normalize-left #t ss goal))
        ((pause? ss) (normalize-left #t ss goal))
        (else (error 'wake-bind (format "invalid ss: ~s" ss)))))

(define (prune-parallel-goal wake? st g)
  (define (pg gx)
    (define ss (prune-goal #f st gx))
    (cond ((not ss) #f)
          ((state? ss) ss)
          ((and (pause? ss) (not (disj? (pause-goal ss))))
           (prune-parallel-goal wake? (pause-state ss) (pause-goal ss)))
          (else (pause st g))))
  (cond ((conj? g)
         (normalize-left wake? (prune-parallel-goal wake? st (conj-c1 g))
                         (conj-c2 g)))
        ((zzz? g) (if wake? (pg ((zzz-wake g))) (pause st g)))
        (else (pg g))))
(define (prune-parallel-pause wake? ss goal)
  (let ((g1 (pause-goal ss))
        (ss2 (prune-parallel-goal wake? (pause-state ss) goal)))
    (cond ((not ss2) #f)
          ((state? ss2) (pause ss2 g1))
          ((pause? ss2) (pause (pause-state ss2)
                               (normalize-left wake? g1 (pause-goal ss2))))
          (else
            (error 'prune-parallel-pause (format "invalid ss2: ~s" ss2))))))

(define (prune force? ss)
  (cond
    ((conj? ss) (prune-bind force? (prune force? (conj-c1 ss)) (conj-c2 ss)))
    ((disj? ss) (prune-mplus force? (prune #f (disj-c1 ss)) (disj-c2 ss)))
    ((pause? ss) (prune-goal force? (pause-state ss) (pause-goal ss)))
    ((not ss) #f)
    (else ss)))
(define (prune-goal force? st goal)
  (cond
    ((conj? goal)
     (prune-bind force? (prune-goal force? st (conj-c1 goal)) (conj-c2 goal)))
    ((disj? goal)
     (prune force? (disj (pause st (disj-c1 goal)) (pause st (disj-c2 goal)))))
    ((zzz? goal)
     (if force?
       (prune-goal (or (eq? #t force?) (and (< 1 force?) (- force? 1)))
                   st ((zzz-wake goal)))
       (pause st goal)))
    ((==? goal) (unify st (==-t1 goal) (==-t2 goal)))
    (else (error 'prune-goal (format "unexpected goal: ~s" ss)))))
(define (prune-mplus force? c1 c2)
  (define (build c1 c2)
    (cond ((not c1) (if force? (prune force? c2) c2))
          ((state? c1) (cons c1 c2))
          ((pair? c1) (cons (car c1) (build (cdr c1) c2)))
          ((not c2) (if force? (prune force? c1) c1))
          ((state? c2) (cons c2 c1))
          ((pair? c2) (cons (car c2) (build c1 (cdr c2))))
          (else (disj c1 c2))))
  (if c1 (let ((c2 (prune #f c2)))
           (cond (c2 (build c1 c2))
                 (force? (prune force? c1))
                 (else c1)))
    (prune force? c2)))
(define (prune-bind force? ss goal)
  (cond ((not ss) #f)
        ((state? ss) (prune-goal force? ss goal))
        ;; NOTE: using force? instead of #f in child prunes.
        ((pair? ss) (prune-bind force? (state*->disj ss) goal))
        ((disj? ss) (normalize-left force? ss goal))
        ((pause? ss) (normalize-left force? ss goal))
        (else (error 'prune-bind (format "invalid ss: ~s" ss)))))

(define (normalize-left prune? lhs g)
  (cond ((not lhs) #f)
        ((state? lhs) (prune-parallel-goal prune? lhs g))
        ((pair? lhs) (normalize-left prune? (state*->disj lhs) g))
        ((disj? lhs)
         (normalize-left-mplus (normalize-left prune? (disj-c1 lhs) g)
                               (normalize-left prune? (disj-c2 lhs) g)))

        ((conj? g)
         (normalize-left prune? (normalize-left prune? lhs (conj-c1 g))
                         (conj-c2 g)))
        ((pause? lhs) (prune-parallel-pause prune? lhs g))
        ((trivial-success? lhs) g)
        (else (conj lhs g))))
(define (normalize-left-mplus ss1 ss2)
  (cond ((not ss1) ss2)
        ((state? ss1) (cons ss1 ss2))
        ((pair? ss1) (cons (car ss1) (normalize-left-mplus (cdr ss1) ss2)))
        ((not ss2) ss1)
        ((state? ss2) (cons ss2 ss1))
        ((pair? ss2) (cons (car ss2) (normalize-left-mplus ss1 (cdr ss2))))
        (else (disj ss1 ss2))))

;; Flattening into DNF is now built into prune.
(define (clean ss) (if (disj? ss) ss (prune #t ss)))

(define (expand-path path path-expected ss0)
  (define ss (clean (wake-path path ss0)))
  (list (cond ((not ss) (error 'expand-path (format "no solution ~s" ss0)))
              ((or (state? ss) (pair? ss)) 'solved)
              ((equal? path path-expected) 'good)
              (else 'unknown))
        ss))

(define (good-path hint ss g*)
  (define (use-hint st) (unify st (==-t1 hint) (==-t2 hint)))
  (define (good-path-goal st g g*)
    (cond ((conj? g) (good-path-goal st (conj-c1 g) (cons (conj-c2 g) g*)))
          ((disj? g) (let ((p1 (good-path-goal st (disj-c1 g) g*)))
                       (if p1 (cons #t p1)
                         (let ((p2 (good-path-goal st (disj-c2 g) g*)))
                           (and p2 (cons #f p2))))))
          ((zzz? g) (and (good-path-goal st ((zzz-wake g)) g*) '()))
          ((==? g)
           (let ((st (unify st (==-t1 g) (==-t2 g))))
             (and st (or (null? g*) (good-path-goal st (car g*) (cdr g*)))
                  '())))
          (else (error 'good-path-goal (format "unexpected goal ~s" g)))))
  (cond ((conj? ss) (good-path hint (conj-c1 ss) (cons (conj-c2 ss) g*)))
        ((disj? ss) (let* ((p1 (good-path hint (disj-c1 ss) g*)))
                      (if p1 (cons #t p1)
                        (let ((p2 (good-path hint (disj-c2 ss) g*)))
                          (and p2 (cons #f p2))))))
        ((pause? ss) (let ((st (use-hint (pause-state ss))))
                       (and st (good-path-goal st (pause-goal ss) g*))))
        (else (error 'good-path (format "unexpected stream ~s" ss)))))

(define (good-paths hint ss)
  (define path (good-path hint ss '()))
  (define next (expand-path path path ss))
  (define flag (car next))
  (define ss-next (cadr next))
  (cons (cons path ss) (if (eq? 'solved flag) '() (good-paths hint ss-next))))

;; Read interact-core.scm to see how this is used.
(define (interact in show out hint ss gpath show?)
  (define (valid-path? path)
    (or (null? path)
        (and (pair? path) (or (eqv? #t (car path)) (eqv? #f (car path)))
             (valid-path? (cdr path)))))
  (define good (if gpath gpath (good-path hint ss '())))
  (when show? (show ss))
  (let ((request (in)))
    (when (not (eof-object? request))
      (cond
        ((eq? 'good-path request)
         (out (list 'good-path good))
         (interact in show out hint ss good #f))
        ((eq? 'steps-remaining request)
         (out (list 'steps-remaining (map car (good-paths hint ss))))
         (interact in show out hint ss good #f))
        ((and (pair? request) (eq? 'jump-to-steps-remaining (car request)))
         (let* ((remaining (good-paths hint ss))
                (remaining-count (length remaining))
                (drop-count (- remaining-count (cadr request)))
                (chosen (and (<= 0 drop-count)
                             (> remaining-count drop-count)
                             (list-ref remaining drop-count))))
           (when (> 0 drop-count)
             (error 'interact (format "only ~s steps remain: ~s"
                                      remaining-count (cadr request))))
           (when (<= remaining-count drop-count)
             (error 'interact "cannot jump to steps-remaining lower than 1"))
           (interact in show out hint (cdr chosen) (car chosen) #t)))
        ((and (pair? request) (valid-path? request))
         (let* ((result (expand-path request good ss))
                (flag (car result))
                (ss2 (cadr result)))
           (out (list 'follow-path flag))
           (when (not (or (eq? 'solved flag) (eq? 'fail-solved flag)))
             (interact in show out hint ss2 #f #t))))
        (else (error 'interact (format "invalid request: ~s" request)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Performance testing interface for biased interleaving search with pruning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define step-count 0)
(define step-report-threshold 1000)
(define (steps-reset) (set! step-count 0))
(define (steps-inc)
  (set! step-count (+ 1 step-count))
  (when (= 0 (remainder step-count step-report-threshold))
    (display (format "~s steps taken" step-count))
    (newline)))

(define (wake-interleave ss)
  (cond
    ((conj? ss)
     (wake-interleave-bind (wake-interleave (conj-c1 ss)) (conj-c2 ss)))
    ((disj? ss)
     (wake-interleave-mplus (wake-interleave (disj-c1 ss)) (disj-c2 ss)))
    ((pause? ss) (wake-interleave-goal (pause-state ss) (pause-goal ss)))
    (else ss)))
(define (wake-interleave-goal st g)
  (cond
    ((conj? g)
     (wake-interleave-bind (wake-interleave-goal st (conj-c1 g)) (conj-c2 g)))
    ((disj? g)
     (wake-interleave-mplus (wake-interleave-goal st (disj-c1 g)) (disj-c2 g)))
    ((zzz? g) (prune-goal prune-depth st ((zzz-wake g))))
    (else (error 'wake-interleave-goal (format "cannot wake ~s" g)))))
(define (wake-interleave-mplus ss c2)
  (cond ((not ss) c2)
        ((state? ss) (cons ss c2))
        ((pair? ss) (cons (car ss) (disj c2 (cdr ss))))
        (else (disj c2 ss))))
(define (wake-interleave-bind ss goal)
  (cond ((not ss) #f)
        ((state? ss) (wake-interleave-goal ss goal))
        ((pair? ss) (wake-interleave-mplus (wake-interleave-goal (car ss) goal)
                                           (conj (cdr ss) goal)))
        (else (conj ss goal))))

(define (stream-next-prune ps)
  (define ss (wake-interleave ps))
  (steps-inc)
  (cond ((not ss) '())
        ((state? ss) (cons ss #f))
        ((pair? ss) ss)
        (else (stream-next-prune ss))))
(define (stream-take-prune n ps)
  (if (and n (= 0 n))
    '()
    (let ((ss (stream-next-prune ps)))
      (if (pair? ss)
        (cons (car ss) (stream-take-prune (and n (- n 1)) (cdr ss)))
        '()))))


#lang racket
(provide run run*
         == =/=
         fresh eigen
         conde conda condu
         symbolo numbero ;; not-pairo
         absento
         project)

;; 4/2/15. Racketized.

;;; 28 November 02014 WEB
;;;
;;; * Fixed missing unquote before E in 'drop-Y-b/c-dup-var'
;;;
;;; * Updated 'rem-xx-from-d' to check against other constraints after
;;; unification, in order to remove redundant disequality constraints
;;; subsumed by absento constraints.

;;; newer version: Sept. 18 2013 (with eigens)
;;; Jason Hemann, Will Byrd, and Dan Friedman
;;; E = (e* . x*)*, where e* is a list of eigens and x* is a list of variables.
;;; Each e in e* is checked for any of its eigens be in any of its x*.  Then it fails.
;;; Since eigen-occurs-check is chasing variables, we might as will do a memq instead
;;; of an eq? when an eigen is found through a chain of walks.  See eigen-occurs-check.
;;; All the e* must be the eigens created as part of a single eigen.  The reifier just
;;; abandons E, if it succeeds.  If there is no failure by then, there were no eigen
;;; violations.

(define empty-c '(() () () () () () ()))

(define eigen-tag (vector 'eigen-tag))

(define-syntax inc
  (syntax-rules ()
    ((_ e) (lambdaf@ () e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax lambdag@
  (syntax-rules (:)
    ((_ (c) e) (lambda (c) e))
    ((_ (c : B E S) e)
     (lambda (c)
       (let ((B (c->B c)) (E (c->E c)) (S (c->S c)))
         e)))
    ((_ (c : B E S D Y N T) e)
     (lambda (c)
       (let ((B (c->B c)) (E (c->E c)) (S (c->S c)) (D (c->D c))
       (Y (c->Y c)) (N (c->N c)) (T (c->T c)))
         e)))))

(define rhs
  (lambda (pr)
    (cdr pr)))
 
(define lhs
  (lambda (pr)
    (car pr)))

(define eigen-var
  (lambda ()
    (vector eigen-tag)))

(define eigen?
  (lambda (x)
    (and (vector? x) (eq? (vector-ref x 0) eigen-tag))))

(define var
  (lambda (dummy)
    (vector dummy)))

(define var?
  (lambda (x)
    (and (vector? x) (not (eq? (vector-ref x 0) eigen-tag)))))

(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define prefix-S
  (lambda (S+ S)
    (cond
      ((eq? S+ S) '())
      (else (cons (car S+)
              (prefix-S (cdr S+) S))))))

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s (unify (cdr u) (cdr v) s))))
        ((or (eigen? u) (eigen? v)) #f)
        ((equal? u v) s)
        (else #f)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) 
         (or 
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))

(define eigen-occurs-check
  (lambda (e* x s)
    (let ((x (walk x s)))
      (cond
        ((var? x) #f)
        ((eigen? x) (memq x e*))
        ((pair? x) 
         (or 
           (eigen-occurs-check e* (car x) s)
           (eigen-occurs-check e* (cdr x) s)))
        (else #f)))))

(define empty-f (lambdaf@ () (mzero)))

(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (cons `(,x . ,v) s)))))

(define unify*  
  (lambda (S+ S)
    (unify (map lhs S+) (map rhs S+) S)))
 
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ((c-inf e))
       (cond
         ((not c-inf) e0)
         ((procedure? c-inf)  (let ((f^ c-inf)) e1))
         ((not (and (pair? c-inf)
                 (procedure? (cdr c-inf))))
          (let ((c^ c-inf)) e2))
         (else (let ((c (car c-inf)) (f (cdr c-inf))) 
                 e3)))))))


(define-syntax eigen
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (c : B E S)
       (let ((x (eigen-var)) ...)
         ((fresh () (eigen-absento `(,x ...) B) g0 g ...) c))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define bind
  (lambda (c-inf g)
    (case-inf c-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((c) (g c))
      ((c f) (mplus (g c) (lambdaf@ () (bind (f) g)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (q) g0 g ...
            (lambdag@ (final-c)
              (let ((z ((reify q) final-c)))
                (choice z empty-f))))
          empty-c))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run n (x) (fresh (q0 q1 q ...) g0 g ... (== `(,q0 ,q1 ,q ...) x))))))
 
(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))
 
(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f) 
         (() '())
         ((f) (take n f))
         ((c) (cons c '()))
         ((c f) (cons c
                  (take (and n (- n 1)) f))))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc 
         (mplus*
           (bind* (g0 c) g ...)
           (bind* (g1 c) g^ ...) ...))))))
 
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0
                    (lambdaf@ () (mplus* e ...))))))
 
(define mplus
  (lambda (c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((c) (choice c f))
      ((c f^) (choice c (lambdaf@ () (mplus (f) f^)))))))


(define c->B (lambda (c) (car c)))
(define c->E (lambda (c) (cadr c)))
(define c->S (lambda (c) (caddr c)))
(define c->D (lambda (c) (cadddr c)))
(define c->Y (lambda (c) (cadddr (cdr c))))
(define c->N (lambda (c) (cadddr (cddr c))))
(define c->T (lambda (c) (cadddr (cdddr c))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
         (ifa ((g0 c) g ...)
              ((g1 c) g^ ...) ...))))))
 
(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* c-inf g ...))
         ((a f) (bind* c-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
         (ifu ((g0 c) g ...)
              ((g1 c) g^ ...) ...))))))
 
(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((c) (bind* c-inf g ...))
         ((c f) (bind* (unit c) g ...)))))))

(define mzero (lambda () #f))

(define unit (lambda (c) c))

(define choice (lambda (c f) (cons c f)))

(define tagged?
  (lambda (S Y y^)
    (ormap (lambda (y) (eqv? (walk y S) y^)) Y)))

(define untyped-var?
  (lambda (S Y N t^)
    (let ((in-type? (lambda (y) (eq? (walk y S) t^))))
      (and (var? t^)
           (not (ormap in-type? Y))
           (not (ormap in-type? N))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (c : B E S)
       (let ((x (walk* x S)) ...)
         ((fresh () g g* ...) c))))))

(define walk*
  (lambda (v S)
    (let ((v (walk v S)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) S) (walk* (cdr v) S)))
        (else v)))))

(define reify-S
  (lambda  (v S)
    (let ((v (walk v S)))
      (cond
        ((var? v)
         (let ((n (length S)))
           (let ((name (reify-name n)))
             (cons `(,v . ,name) S))))
        ((pair? v)
         (let ((S (reify-S (car v) S)))
           (reify-S (cdr v) S)))
        (else S)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define drop-dot
  (lambda (X)
    (map (lambda (t)
           (let ((a (lhs t))
                 (d (rhs t)))
             `(,a ,d)))
         X)))

(define sorter
  (lambda (ls)
    (sort ls lex<=?)))
                              
(define lex<=?
  (lambda (x y)
    (string<=? (datum->string x) (datum->string y))))

(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))

(define anyvar? 
  (lambda (u r)
    (cond
      ((pair? u)
       (or (anyvar? (car u) r)
           (anyvar? (cdr u) r)))
      (else (var? (walk u r))))))

(define anyeigen? 
  (lambda (u r)
    (cond
      ((pair? u)
       (or (anyeigen? (car u) r)
           (anyeigen? (cdr u) r)))
      (else (eigen? (walk u r))))))

(define member* 
  (lambda (u v)
    (cond
      ((equal? u v) #t)
      ((pair? v)
       (or (member* u (car v)) (member* u (cdr v))))
      (else #f))))

;;;

(define (find f l)
  (cond [(memf f l) => car] [else #f]))

(define drop-N-b/c-const
  (lambdag@ (c : B E S D Y N T)
    (let ((const? (lambda (n)
                    (not (var? (walk n S))))))
      (cond
        ((find const? N) =>
         (lambda (n) `(,B ,E ,S ,D ,Y ,(remq1 n N) ,T)))
        (else c)))))

(define drop-Y-b/c-const
  (lambdag@ (c : B E S D Y N T)
    (let ((const? (lambda (y)
                    (not (var? (walk y S))))))
      (cond
  ((find const? Y) =>
         (lambda (y) `(,B ,E ,S ,D ,(remq1 y Y) ,N ,T)))
        (else c)))))

(define remq1
  (lambda (elem ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) elem) (cdr ls))
      (else (cons (car ls) (remq1 elem (cdr ls)))))))

(define same-var?
  (lambda (v)
    (lambda (v^)
      (and (var? v) (var? v^) (eq? v v^)))))

(define find-dup
  (lambda (f S)
    (lambda (set)
      (let loop ((set^ set))
        (cond
          ((null? set^) #f)
          (else
           (let ((elem (car set^)))
             (let ((elem^ (walk elem S)))
               (cond
                 ((find (lambda (elem^^)
                          ((f elem^) (walk elem^^ S)))
                        (cdr set^))
                  elem)
                 (else (loop (cdr set^))))))))))))

(define drop-N-b/c-dup-var
  (lambdag@ (c : B E S D Y N T)
    (cond
      (((find-dup same-var? S) N) =>
       (lambda (n) `(,B ,E ,S ,D ,Y ,(remq1 n N) ,T)))
      (else c))))

(define drop-Y-b/c-dup-var
  (lambdag@ (c : B E S D Y N T)
    (cond
      (((find-dup same-var? S) Y) =>
       (lambda (y)
         `(,B ,E ,S ,D ,(remq1 y Y) ,N ,T)))
      (else c))))

(define var-type-mismatch?
  (lambda (S Y N t1^ t2^)
    (cond
      ((num? S N t1^) (not (num? S N t2^)))
      ((sym? S Y t1^) (not (sym? S Y t2^)))
      (else #f))))

(define term-ununifiable?
  (lambda (S Y N t1 t2)
    (let ((t1^ (walk t1 S))
          (t2^ (walk t2 S)))
      (cond
        ((or (untyped-var? S Y N t1^) (untyped-var? S Y N t2^)) #f)
        ((var? t1^) (var-type-mismatch? S Y N t1^ t2^))
        ((var? t2^) (var-type-mismatch? S Y N t2^ t1^))
        ((and (pair? t1^) (pair? t2^))
         (or (term-ununifiable? S Y N (car t1^) (car t2^))
             (term-ununifiable? S Y N (cdr t1^) (cdr t2^))))
        (else (not (eqv? t1^ t2^)))))))

(define T-term-ununifiable?
  (lambda (S Y N)
    (lambda (t1)
      (let ((t1^ (walk t1 S)))
        (letrec
            ((t2-check
              (lambda (t2)
                (let ((t2^ (walk t2 S)))
                  (cond
                    ((pair? t2^) (and
                                  (term-ununifiable? S Y N t1^ t2^)
                                  (t2-check (car t2^))
                                  (t2-check (cdr t2^))))
                    (else (term-ununifiable? S Y N t1^ t2^)))))))
          t2-check)))))

(define num?
  (lambda (S N n)
    (let ((n (walk n S)))
      (cond
        ((var? n) (tagged? S N n))
        (else (number? n))))))

(define sym?
  (lambda (S Y y)
    (let ((y (walk y S)))          
      (cond
        ((var? y) (tagged? S Y y))
        (else (symbol? y))))))

(define drop-T-b/c-Y-and-N
  (lambdag@ (c : B E S D Y N T)
    (let ((drop-t? (T-term-ununifiable? S Y N)))
      (cond
        ((find (lambda (t) ((drop-t? (lhs t)) (rhs t))) T) =>
         (lambda (t) `(,B ,E ,S ,D ,Y ,N ,(remq1 t T))))
        (else c)))))

(define move-T-to-D-b/c-t2-atom
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((ormap (lambda (t)
               (let ((t2^ (walk (rhs t) S)))
                 (cond
                   ((and (not (untyped-var? S Y N t2^))
                         (not (pair? t2^)))
                    (let ((T (remq1 t T)))
                      `(,B ,E ,S ((,t) . ,D) ,Y ,N ,T)))
                   (else #f))))
             T))
      (else c))))

(define terms-pairwise=?
  (lambda (pr-a^ pr-d^ t-a^ t-d^ S)
    (or
     (and (term=? pr-a^ t-a^ S)
          (term=? pr-d^ t-a^ S))
     (and (term=? pr-a^ t-d^ S)
          (term=? pr-d^ t-a^ S)))))

(define T-superfluous-pr?
  (lambda (S Y N T)
    (lambda (pr)
      (let ((pr-a^ (walk (lhs pr) S))
            (pr-d^ (walk (rhs pr) S)))
        (cond
          ((ormap
               (lambda (t)
                 (let ((t-a^ (walk (lhs t) S))
                       (t-d^ (walk (rhs t) S)))
                   (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S)))
             T)
           (andmap
            (lambda (t)
              (let ((t-a^ (walk (lhs t) S))
                    (t-d^ (walk (rhs t) S)))
                (or
                 (not (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S))
                 (untyped-var? S Y N t-d^)
                 (pair? t-d^))))
            T))
          (else #f))))))

(define drop-from-D-b/c-T
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((find
           (lambda (d)
             (ormap
                 (T-superfluous-pr? S Y N T)
               d))
         D) =>
         (lambda (d) `(,B ,E ,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define drop-t-b/c-t2-occurs-t1
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((find (lambda (t)
               (let ((t-a^ (walk (lhs t) S))
                     (t-d^ (walk (rhs t) S)))
                 (mem-check t-d^ t-a^ S)))
             T) =>
             (lambda (t)
               `(,B ,E ,S ,D ,Y ,N ,(remq1 t T))))
      (else c))))

(define split-t-move-to-d-b/c-pair
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((ormap
         (lambda (t)
           (let ((t2^ (walk (rhs t) S)))
             (cond
               ((pair? t2^) (let ((ta `(,(lhs t) . ,(car t2^)))
                                  (td `(,(lhs t) . ,(cdr t2^))))
                              (let ((T `(,ta ,td . ,(remq1 t T))))
                                `(,B ,E ,S ((,t) . ,D) ,Y ,N ,T))))
               (else #f))))
         T))
      (else c))))

(define find-d-conflict
  (lambda (S Y N)
    (lambda (D)
      (find
       (lambda (d)
   (ormap (lambda (pr)
       (term-ununifiable? S Y N (lhs pr) (rhs pr)))
     d))
       D))))

(define drop-D-b/c-Y-or-N
  (lambdag@ (c : B E S D Y N T)
    (cond
      (((find-d-conflict S Y N) D) =>
       (lambda (d) `(,B ,E ,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define cycle
  (lambdag@ (c)
    (let loop ((c^ c)
               (fns^ (LOF))
               (n (length (LOF))))
      (cond
        ((zero? n) c^)
        ((null? fns^) (loop c^ (LOF) n))
        (else
         (let ((c^^ ((car fns^) c^)))
           (cond
             ((not (eq? c^^ c^))                                    
              (loop c^^ (cdr fns^) (length (LOF))))
             (else (loop c^ (cdr fns^) (sub1 n))))))))))

(define absento
  (lambda (u v)
    (lambdag@ (c : B E S D Y N T)
      (cond
        [(mem-check u v S) (mzero)]
        [else (unit `(,B ,E ,S ,D ,Y ,N ((,u . ,v) . ,T)))]))))

(define eigen-absento
  (lambda (e* x*)
    (lambdag@ (c : B E S D Y N T)
      (cond
        [(eigen-occurs-check e* x* S) (mzero)]
        [else (unit `(,B ((,e* . ,x*) . ,E) ,S ,D ,Y ,N ,T))]))))

(define mem-check
  (lambda (u t S)
    (let ((t (walk t S)))
      (cond
        ((pair? t)
         (or (term=? u t S)
             (mem-check u (car t) S)
             (mem-check u (cdr t) S)))
        (else (term=? u t S))))))

(define term=?
  (lambda (u t S)
    (cond
      ((unify u t S) =>
       (lambda (S0)
         (eq? S0 S)))
      (else #f))))

(define ground-non-<type>?
  (lambda (pred)
    (lambda (u S)
      (let ((u (walk u S)))
        (cond
          ((var? u) #f)
          (else (not (pred u))))))))
;; moved 
(define ground-non-symbol?
  (ground-non-<type>? symbol?))

(define ground-non-number?
  (ground-non-<type>? number?))

(define symbolo
  (lambda (u)
    (lambdag@ (c : B E S D Y N T)
      (cond
        [(ground-non-symbol? u S) (mzero)]
        [(mem-check u N S) (mzero)]
        [else (unit `(,B ,E ,S ,D (,u . ,Y) ,N ,T))]))))

(define numbero 
  (lambda (u)
    (lambdag@ (c : B E S D Y N T)
      (cond
        [(ground-non-number? u S) (mzero)]
        [(mem-check u Y S) (mzero)]
        [else (unit `(,B ,E ,S ,D ,Y (,u . ,N) ,T))]))))
;; end moved

(define =/= ;; moved
  (lambda (u v)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((unify u v S) =>
         (lambda (S0)
           (let ((pfx (prefix-S S0 S)))
             (cond
               ((null? pfx) (mzero))
               (else (unit `(,B ,E ,S (,pfx . ,D) ,Y ,N ,T)))))))
        (else c)))))

(define ==
  (lambda (u v)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((unify u v S) =>
         (lambda (S0)
           (cond
             ((==fail-check B E S0 D Y N T) (mzero))
             (else (unit `(,B ,E ,S0 ,D ,Y ,N ,T))))))
        (else (mzero))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define ==fail-check
  (lambda (B E S0 D Y N T)
    (cond
      ((eigen-absento-fail-check E S0) #t)
      ((atomic-fail-check S0 Y ground-non-symbol?) #t)
      ((atomic-fail-check S0 N ground-non-number?) #t)
      ((symbolo-numbero-fail-check S0 Y N) #t)
      ((=/=-fail-check S0 D) #t)
      ((absento-fail-check S0 T) #t)
      (else #f))))

(define eigen-absento-fail-check
  (lambda (E S0)
    (ormap (lambda (e*/x*) (eigen-occurs-check (car e*/x*) (cdr e*/x*) S0)) E)))

(define atomic-fail-check
  (lambda (S A pred)
    (ormap (lambda (a) (pred (walk a S) S)) A)))

(define symbolo-numbero-fail-check
  (lambda (S A N)
    (let ((N (map (lambda (n) (walk n S)) N)))
      (ormap (lambda (a) (ormap (same-var? (walk a S)) N))
        A))))

(define absento-fail-check
  (lambda (S T)
    (ormap (lambda (t) (mem-check (lhs t) (rhs t) S)) T)))

(define =/=-fail-check
  (lambda (S D)
    (ormap (d-fail-check S) D)))

(define d-fail-check
  (lambda (S)
    (lambda (d)
      (cond
        ((unify* d S) =>
   (lambda (S+) (eq? S+ S)))
        (else #f)))))

(define reify
  (lambda (x)
    (lambda (c)
      (let ((c (cycle c)))
        (let* ((S (c->S c))
             (D (walk* (c->D c) S))
             (Y (walk* (c->Y c) S))
             (N (walk* (c->N c) S))
             (T (walk* (c->T c) S)))
        (let ((v (walk* x S)))
          (let ((R (reify-S v '())))
            (reify+ v R
                    (let ((D (filter-not
                              (lambda (d)
                                (let ((dw (walk* d S)))
                                  (or
                                    (anyvar? dw R)
                                    (anyeigen? dw R))))
                               (rem-xx-from-d c))))
                      (rem-subsumed D)) 
                    (filter-not
                     (lambda (y) (var? (walk y R)))
                     Y)
                    (filter-not
                     (lambda (n) (var? (walk n R)))
                     N)
                    (filter-not (lambda (t)
                            (or (anyeigen? t R) (anyvar? t R))) T)))))))))

(define reify+
  (lambda (v R D Y N T)
    (form (walk* v R)
          (walk* D R)
          (walk* Y R)
          (walk* N R)
          (rem-subsumed-T (walk* T R)))))

(define form
  (lambda (v D Y N T)
    (let ((fd (sort-D D))
          (fy (sorter Y))
          (fn (sorter N))
          (ft (sorter T)))
      (let ((fd (if (null? fd) fd
                    (let ((fd (drop-dot-D fd)))
                      `((=/= . ,fd)))))
            (fy (if (null? fy) fy `((sym . ,fy))))
            (fn (if (null? fn) fn `((num . ,fn))))
            (ft (if (null? ft) ft
                    (let ((ft (drop-dot ft)))
                      `((absento . ,ft))))))
        (cond
          ((and (null? fd) (null? fy)
                (null? fn) (null? ft))
           v)
          (else (append `(,v) fd fn fy ft)))))))

(define sort-D
  (lambda (D)
    (sorter
     (map sort-d D))))

(define sort-d
  (lambda (d)
    (sort
     (map sort-pr d)
     (lambda (x y)
       (lex<=? (car x) (car y))))))

(define drop-dot-D
  (lambda (D)
    (map drop-dot D)))

(define lex<-reified-name?
  (lambda (r)
    (char<?
     (string-ref
      (datum->string r) 0)
     #\_)))

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
        ((lex<-reified-name? r) pr)
        ((lex<=? r l) `(,r . ,l))
        (else pr)))))

(define rem-subsumed
  (lambda (D)
    (let rem-subsumed ((D D) (d^* '()))
      (cond
        ((null? D) d^*)
        ((or (subsumed? (car D) (cdr D))
             (subsumed? (car D) d^*))
         (rem-subsumed (cdr D) d^*))
        (else (rem-subsumed (cdr D)
                (cons (car D) d^*)))))))
 
(define subsumed?
  (lambda (d d*)
    (cond
      ((null? d*) #f)
      (else
        (let ((d^ (unify* (car d*) d)))
          (or
            (and d^ (eq? d^ d))
            (subsumed? d (cdr d*))))))))

(define rem-xx-from-d
  (lambdag@ (c : B E S D Y N T)
    (let ((D (walk* D S)))
      (filter-not not ;; eh? 
            (map (lambda (d)
                   (cond
                     ((unify* d S) =>
                      (lambda (S0)
                        (cond
                          ((==fail-check B E S0 '() Y N T) #f)
                          (else (prefix-S S0 S)))))
                     (else #f)))
                 D)))))

(define rem-subsumed-T 
  (lambda (T)
    (let rem-subsumed ((T T) (T^ '()))
      (cond
        ((null? T) T^)
        (else
         (let ((lit (lhs (car T)))
               (big (rhs (car T))))
           (cond
             ((or (subsumed-T? lit big (cdr T))
                  (subsumed-T? lit big T^))
              (rem-subsumed (cdr T) T^))
             (else (rem-subsumed (cdr T)
                     (cons (car T) T^))))))))))

(define subsumed-T? 
  (lambda (lit big T)
    (cond
      ((null? T) #f)
      (else
       (let ((lit^ (lhs (car T)))
             (big^ (rhs (car T))))
         (or
           (and (eq? big big^) (member* lit^ lit))
           (subsumed-T? lit big (cdr T))))))))

(define LOF
  (lambda ()
    `(,drop-N-b/c-const ,drop-Y-b/c-const ,drop-Y-b/c-dup-var
      ,drop-N-b/c-dup-var ,drop-D-b/c-Y-or-N ,drop-T-b/c-Y-and-N
      ,move-T-to-D-b/c-t2-atom ,split-t-move-to-d-b/c-pair
      ,drop-from-D-b/c-T ,drop-t-b/c-t2-occurs-t1)))
