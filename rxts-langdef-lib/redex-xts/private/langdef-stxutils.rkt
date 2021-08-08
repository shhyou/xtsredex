#lang racket/base

(provide
 insert-captured-reference
 flatten-identifier-tree)

(define (insert-captured-reference stx name)
  (syntax-property (syntax-local-introduce (datum->syntax stx name stx))
                   'original-for-check-syntax
                   #t))

(define (flatten-identifier-tree datum)
  (cond
    [(identifier? datum) (list datum)]
    [(syntax? datum) (flatten-identifier-tree (syntax-e datum))]
    [(null? datum) '()]
    [(pair? datum) (append (flatten-identifier-tree (car datum))
                           (flatten-identifier-tree (cdr datum)))]
    [else (list datum)]))
