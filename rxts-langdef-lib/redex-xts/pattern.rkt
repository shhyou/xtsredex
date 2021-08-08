#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "private/langdef-stxutils.rkt")
         racket/match
         redex/reduction-semantics)

(provide (all-defined-out)
         (for-syntax redex-pattern))

(define redexpat-any 'any)
(define redexpat-_ '_)
(define redexpat-integer 'integer)
(define redexpat-boolean 'boolean)
(define redexpat-variable 'variable)
(define redexpat-variable-prefix 'variable-prefix)
(define redexpat-variable-not-otherwise-mentioned 'variable-not-otherwise-mentioned)

(define redex-pattern-keywords
  '(any
    _
    number
    natural
    integer
    real
    string
    boolean
    variable
    variable-except
    variable-prefix
    variable-not-otherwise-mentioned
    hole
    name
    in-hole
    hide-hole
    side-condition
    compatible-closure-context
    cross))

(define redex-pattern-opening-keyword-list
  '(variable-except
    variable-prefix
    name
    in-hole
    hide-hole
    side-condition
    compatible-closure-context
    cross))

(struct p:atomic (pattern)
  #:transparent)
(struct p:literal (symbol)
  #:transparent)
(struct p:datum (value)
  #:transparent)
(struct p:nonterminal (name)
  #:transparent)
(struct p:repeat (pattern)
  #:transparent)
(struct p:list (patterns)
  #:transparent)
(struct p:unsupported (pattern)
  #:transparent)

(define (split-non-terminal nt-sym)
  (define s (symbol->string nt-sym))
  (cond
    [(regexp-match #rx"^([^_]+)_(.*)$" s)
     => (match-lambda
          [(list lexeme nt suffix)
           (values (string->symbol nt) suffix)])]
    [else
     (values (string->symbol s) "")]))

(define (parse-pattern nonterminals pattern)
  (cond
    [(and (pair? pattern)
          (not (member (car pattern) redex-pattern-opening-keyword-list)))
     (p:list
      (let loop ([patterns pattern])
        (match patterns
          ['() '()]
          [(cons pattern (cons '... patterns))
           (cons (p:repeat (parse-pattern nonterminals pattern))
                 (loop patterns))]
          [(cons pattern patterns)
           (cons (parse-pattern nonterminals pattern)
                 (loop patterns))])))]
    [else
     (match pattern
       [(or 'any '_ 'number 'natural 'integer 'real 'string 'boolean)
        (p:atomic pattern)]
       [(or 'variable `(variable-except ,_ ...) `(variable-prefix ,_))
        (p:atomic pattern)]
       ['hole
        (p:atomic pattern)]
       [(? symbol?)
        (cond
          [(member pattern nonterminals)
           (p:nonterminal pattern)]
          [else
           (p:literal pattern)])]
       [(? (λ (datum)
             (not (pair? datum))))
        (p:datum pattern)]
       [_
        (p:unsupported pattern)])]))

(begin-for-syntax
  (define-syntax-class redex-pattern
    #:commit
    #:attributes (uses)
    (pattern (~datum any)
             #:with uses (insert-captured-reference
                          this-syntax
                          'redexpat-any))
    (pattern (~datum _)
             #:with uses (insert-captured-reference
                          this-syntax
                          'redexpat-_))
    (pattern (~datum integer)
             #:with uses (insert-captured-reference
                          this-syntax
                          'redexpat-integer))
    (pattern (~datum boolean)
             #:with uses (insert-captured-reference
                          this-syntax
                          'redexpat-boolean))
    (pattern (~datum variable)
             #:with uses (insert-captured-reference
                          this-syntax
                          'redexpat-variable))
    (pattern ((~and (~datum variable-prefix)
                    variable-prefix-stx)
              prefix:id)
             #:with uses (insert-captured-reference
                          #'variable-prefix-stx
                          'redexpat-variable-prefix))
    (pattern (~datum variable-not-otherwise-mentioned)
             #:with uses (insert-captured-reference
                          this-syntax
                          'redexpat-variable-not-otherwise-mentioned))
    (pattern (~datum hole)
             #:with uses (insert-captured-reference
                          this-syntax
                          'hole))
    (pattern (subpattern:redex-pattern ...)
             #:with uses #'(subpattern.uses ...))
    (pattern pat:expr
             #:with uses '()))
  )
