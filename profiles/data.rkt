#lang racket

(require (for-syntax racket/list
                     racket/match
                     "../lib/structs.rkt")
         "../positive.rkt")

(provide #%module-begin data)

(define-for-syntax (synthetic-abstraction-types l)
  (map
   (λ (ts)
     (match ts
       [(list t1 t2 t3)
        (let
            ([name
              (datum->syntax #f (string->symbol (string-append "abstr-"
                                                               (symbol->string (syntax->datum t1))
                                                               "-"
                                                               (symbol->string (syntax->datum t2))
                                                               "-"
                                                               (symbol->string (syntax->datum t3)))))])
          #`(#,name ([#,name (#,t1 #,t2) (#,t3)])))]))     
   (cartesian-product l l l)))

(define-for-syntax PROFILE (profile
                            (limitation "lambda"
                                        "abstr-"
                                        "Only synthetic abstraction types allowed (encode first-order fun.s)")))

(define-syntax (data stx)
  (syntax-case stx ()
    [(data #s(recursive) (name ((con (type ...)) ...)) ...)
     (data-helper #t PROFILE #`(data (name ((con (type ...) ()) ...)) ...
                                     #,@(synthetic-abstraction-types (syntax->list #'(name ...)))))]
    [(data (name ((con (type ...)) ...)) ...)
     (data-helper #f PROFILE #`(data (name ((con (type ...) ()) ...)) ...
                                     #,@(synthetic-abstraction-types (syntax->list #'(name ...)))))]))
