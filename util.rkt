#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience macros to define contract and/or provide (similar to
;; define/contract).

(define-syntax define/contract/provide
  (syntax-rules ()
    [(_ (id . args) contract body ...)
     (begin
       (define/contract (id . args) contract body ...)
       (provide/contract [id contract]))]
    [(_ id contract expr)
     (begin
       (define/contract id contract expr)
       (provide/contract [id contract]))] ))

(define-syntax define/provide
  (syntax-rules ()
    [(_ (id . args) body ...)
     (begin
       (define (id . args) body ...)
       (provide id))]
    [(_ id expr)
     (begin
       (define id expr)
       (provide id))] ))

(provide define/contract/provide
         define/provide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (any/c ... -> string?)  Given any number of Racket identifiers or
;; literals, convert them to a string. Non-literals are printed as
;; "expression = value".  For use with e.g. log-debug which takes a
;; single string? not format or printf style, plus, we want to show
;; the expression = value thing, too.
(define-syntax tr
  (syntax-rules ()
    [(_ e)
     (if (or (string? (syntax-e #'e))
             (number? (syntax-e #'e)))
         (format "~a" e)
         (format "~s=~a"
                 (syntax->datum #'e)
                 e))]
    [(_ e0 e1 ...)
     (string-append (tr e0)
                    " "
                    (tr e1 ...))]))

(provide tr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-logger http)
(provide log-http-fatal
         log-http-error
         log-http-warning
         log-http-info
         log-http-debug)
