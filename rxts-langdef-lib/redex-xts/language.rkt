#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "private/langdef-stxutils.rkt")

         racket/list
         racket/set
         racket/hash

         racket/match

         syntax/parse/define

         redex/reduction-semantics

         "pattern.rkt")

(provide (all-defined-out)
         (all-from-out "pattern.rkt"))

(begin-for-syntax
  (define-syntax-class non-terminal-def
    #:commit
    #:attributes ([non-terminal-name 1]
                  [pattern 1]
                  uses)
    (pattern (non-terminal-name:id ...+ (~literal ::=) pattern:redex-pattern ...+)
             #:with uses #'(pattern.uses ...))
    (pattern (one-non-terminal-name:id pattern:redex-pattern ...+)
             #:with uses #'(pattern.uses ...)
             #:with (non-terminal-name ...) #'(one-non-terminal-name))
    (pattern ((non-terminal-name:id ...+) pattern:redex-pattern ...+)
             #:with uses #'(pattern.uses ...)))
  )

(define-syntax-parse-rule (define-language/xts lang-name:id
                            (~optional
                             (~seq #:define-abstract-syntax
                                   ast-name:id))
                            nts:non-terminal-def ...
                            (~optional
                             (~seq #:binding-forms
                                   binding-forms ...)))
  #:with def-lang
  (syntax-property
   (syntax/loc this-syntax
     (define-language lang-name
       nts ...
       (~? (~@ #:binding-forms binding-forms ...))))
   'disappeared-use
   (flatten-identifier-tree #'(nts.uses ...)))
  (begin
    (~? (define ast-name
          (build-language-definition
           #:name 'lang-name
           #:non-terminals-list '((nts.non-terminal-name ...) ...)
           #:productions-list '((nts.pattern ...) ...))))
    def-lang))

(struct redex-language
  (name literals nonterminals production-map)
  #:transparent)
(struct nonterminal
  (names productions)
  #:transparent)

(define (build-language-definition
         #:name name
         #:non-terminals-list non-terminals-list
         #:productions-list productions-list)
  (define all-nonterminals
    (apply append non-terminals-list))
  (define all-symbols
    (flatten productions-list))
  (redex-language
   name
   (set->list
    (set-subtract (apply set all-symbols)
                  (apply set all-nonterminals)
                  (apply set redex-pattern-keywords)))
   (map car non-terminals-list)
   (apply hash-union
          (for/list ([non-terminals (in-list non-terminals-list)]
                     [productions (in-list productions-list)])
            (define nt-data
              (nonterminal
               non-terminals
               (for/list ([production (in-list productions)])
                 (parse-pattern all-nonterminals production))))
            (for/hash ([a-non-terminal (in-list non-terminals)])
              (values a-non-terminal nt-data))))))
