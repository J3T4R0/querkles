#lang racket/base
;https://lexi-lambda.github.io/hackett/reference-typeclasses.html#%28part._reference-semigroup-monoid%29
(require (for-meta 2 racket/base
                     syntax/parse)
         (for-syntax racket/base
                     racket/list
                     racket/trace
                     syntax/kerncase
                     threading)
         racket/sequence
         syntax/parse/define)

(begin-for-syntax
  ; Like syntax/loc and friends, but copy properties from the source syntax object in addition to
  ; source location.
  (define-syntaxes [syntax/loc/props quasisyntax/loc/props template/loc/props quasitemplate/loc/props]
    (let ()
      (define (make-syntax/loc/props name syntax-id)
        (syntax-parser
          [(_ from-stx-expr:expr {~describe "template" template})
           #`(let ([from-stx from-stx-expr])
               (unless (syntax? from-stx)
                 (raise-argument-error '#,name "syntax?" from-stx))
               (let* ([stx (#,syntax-id template)]
                      [stx* (syntax-disarm stx #f)])
                 (syntax-rearm (datum->syntax stx* (syntax-e stx*) from-stx from-stx) stx)))]))
      (values (make-syntax/loc/props 'syntax/loc/props #'syntax)
              (make-syntax/loc/props 'quasisyntax/loc/props #'quasisyntax)
              (make-syntax/loc/props 'template/loc/props #'template)
              (make-syntax/loc/props 'quasitemplate/loc/props #'quasitemplate))))
  (define current-context (make-parameter #f))
  (define current-stop-list (make-parameter (list #'define-values #'define-syntaxes #'for)))
  (define current-intdef-ctx (make-parameter #f))

  (define (current-expand stx)
    (syntax-disarm (local-expand (syntax-disarm stx #f)
                                 (current-context)
                                 (current-stop-list)
                                 (current-intdef-ctx))
                   #f))

  (define-syntax-class plain-formals
    #:description "formals"
    #:attributes [[id 1]]
    #:commit
    [pattern (id:id ...)]
    [pattern (id*:id ... . id**:id) #:with [id ...] #'[id* ... id**]])

  (define-syntax-class lambda-clause
    #:description #f
    #:attributes [expansion]
    #:commit
    [pattern [formals:plain-formals body ...]
             #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
                   (syntax-local-bind-syntaxes (attribute formals.id) #f intdef-ctx)]
             #:with formals* (internal-definition-context-introduce intdef-ctx #'formals)
             #:with body* (parameterize ([current-intdef-ctx intdef-ctx])
                            (expand-body/in-ctx (attribute body) intdef-ctx))
             #:attr expansion #'[formals* body*]])

  (define (expand-expression stx)
    (syntax-parse (parameterize ([current-context 'expression])
                    (current-expand stx))
      #:literal-sets [kernel-literals]
      #:literals [for]
      [({~or quote quote-syntax #%top #%variable-reference} ~! . _)
       this-syntax]

      [({~and head {~or #%expression #%plain-app begin begin0 if with-continuation-mark}} ~! form ...)
       #:with [form* ...] (map expand-expression (attribute form))
       (syntax/loc/props this-syntax
         (head form* ...))]

      [(head:#%plain-lambda ~! . clause:lambda-clause)
       (syntax/loc/props this-syntax
         (head . clause.expansion))]

      [(head:case-lambda ~! clause:lambda-clause ...)
       (syntax/loc/props this-syntax
         (head clause.expansion ...))]

      [({~or {~or {~and head:let-values ~! {~bind [rec? #f] [stxs? #f]}}
                  {~and head:letrec-values ~! {~bind [rec? #t] [stxs? #f]}}}
             {~seq head:letrec-syntaxes+values {~bind [rec? #t] [stxs? #t]}
                   ~! ([(x/s:id ...) rhs/s] ...)}}
        ([(x:id ...) rhs] ...) body ...)
       #:do [(define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
             (syntax-local-bind-syntaxes (append* (attribute x)) #f intdef-ctx)
             (when (attribute stxs?)
               (for ([xs/s (in-list (attribute x/s))]
                     [rhs/s (in-list (attribute rhs/s))])
                 (syntax-local-bind-syntaxes xs/s rhs/s intdef-ctx)))]
       #:with [[x* ...] ...] (internal-definition-context-introduce intdef-ctx #'[[x ...] ...])
       #:with [rhs* ...] (if (attribute rec?)
                             (parameterize ([current-intdef-ctx intdef-ctx])
                               (map expand-expression (attribute rhs)))
                             (map expand-expression (attribute rhs)))
       #:with body* (parameterize ([current-intdef-ctx intdef-ctx])
                      (expand-body/in-ctx (attribute body) intdef-ctx))
       (if (attribute stxs?)
           (~> (syntax/loc this-syntax
                 (letrec-values ([(x* ...) rhs*] ...) body*))
               (syntax-track-origin this-syntax #'head))
           (syntax/loc/props this-syntax
             (head ([(x* ...) rhs*] ...) body*)))]

      [(head:for ([x:id seq:expr] ...) body ...+)
       (syntax/loc/props this-syntax
         (head ([x (in-list (reverse (sequence->list seq)))] ...)
           body ...))]

      [_
       this-syntax]))

  (define (expand-body/in-ctx stxs ctx)
    (define (add-ctx-scope stx)
      (internal-definition-context-introduce ctx stx 'add))
    (parameterize ([current-intdef-ctx ctx])
      (add-ctx-scope (expand-body (map add-ctx-scope stxs)))))

  (define (expand-body stxs)
    (define intdef-ctx (syntax-local-make-definition-context (current-intdef-ctx)))
    (parameterize ([current-context (list (gensym))]
                   [current-intdef-ctx intdef-ctx])
      (define-values [binding-clauses exprs disappeared-uses disappeared-bindings]
        (let loop ([stxs stxs]
                   [binding-clauses '()]
                   [exprs '()]
                   [disappeared-uses '()]
                   [disappeared-bindings '()])
          (if (empty? stxs)
              (values (reverse binding-clauses) (reverse exprs) disappeared-uses disappeared-bindings)
              (syntax-parse (current-expand (first stxs))
                #:literal-sets [kernel-literals]
                [(head:begin ~! form ...)
                 (loop (append (for/list ([form (in-list (attribute form))])
                                 (syntax-track-origin form this-syntax #'head))
                               stxs)
                       binding-clauses exprs disappeared-uses disappeared-bindings)]
                [(head:define-values ~! [x:id ...] rhs)
                 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
                 #:do [(syntax-local-bind-syntaxes (attribute x*) #f intdef-ctx)]
                 (loop
                  (rest stxs)
                  (cons (syntax-track-origin #'[(x* ...) rhs] this-syntax #'head) binding-clauses)
                  exprs disappeared-uses disappeared-bindings)]
                [(head:define-syntaxes ~! [x:id ...] rhs)
                 #:with [x* ...] (map syntax-local-identifier-as-binding (attribute x))
                 #:do [(syntax-local-bind-syntaxes (attribute x*) #'rhs intdef-ctx)]
                 (loop (rest stxs) binding-clauses exprs
                       (cons #'head disappeared-uses) (cons (attribute x*) disappeared-bindings))]
                [_
                 (loop (rest stxs) binding-clauses (cons this-syntax exprs)
                       disappeared-uses disappeared-bindings)]))))
      (define expanded-binding-clauses
        (for/list ([binding-clause (in-list binding-clauses)])
          (syntax-parse binding-clause
            [[(x ...) rhs]
             (quasisyntax/loc/props this-syntax
               [(x ...) #,(expand-expression #'rhs)])])))
      (define expanded-exprs (map expand-expression exprs))
      (~> #`(letrec-values #,expanded-binding-clauses #,@expanded-exprs)
          (syntax-property 'disappeared-uses disappeared-uses)
          (syntax-property 'disappeared-bindings disappeared-bindings)))))

(define-simple-macro (print-up-to n)
  (for ([i (in-range n)])
    (println i)))

(define-syntax-parser hijack-for-loops
  [(_ form:expr) (expand-expression #'form)])