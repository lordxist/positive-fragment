#lang racket

(require (only-in "../positive.rkt" [data posdata])
         (for-syntax racket/bool))

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

(define-syntax (data stx)
  (syntax-case stx ()
    [(data (name ((con (type ...)) ...)) ...)
     #`(begin
         (provide (rename-out [module-begin #%module-begin]))
         (posdata #s(preprocess) (name ((con (type ...) ()) ...)) ...)
         #,module-begin-def)]))
