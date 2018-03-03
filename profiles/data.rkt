#lang racket

(require (only-in "../positive.rkt" [data posdata])
         (for-syntax racket/bool
                     racket/list
                     racket/match))

(provide #%module-begin data)

(define-for-syntax (convert-calls stx)
  (syntax-case stx ()
    [(synt-elem ...)
     #`(#,@(map convert-calls (syntax->list #'(synt-elem ...))))]
    [synt-elem
     (and (symbol? (syntax->datum #'synt-elem))
          (symbol=? 'call (syntax->datum #'synt-elem)))
     #'cmd]
    [synt-elem
     #'synt-elem]))

(define-for-syntax module-begin-def
  #`(...(define-syntax (module-begin stx)
          (syntax-case stx ()
            [(_ elem ...)
             #`(#%module-begin #,@(map convert-calls (syntax->list #'(elem ...))))]))))

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

(define-syntax (data stx)
  (syntax-case stx ()
    [(data (name ((con (type ...)) ...)) ...)
     #`(begin
         (provide (rename-out [module-begin #%module-begin]))
         (posdata #s(preprocess)
                  (name ((con (type ...) ()) ...)) ...
                  #,@(synthetic-abstraction-types (syntax->list #'(name ...))))
         #,module-begin-def)]))
