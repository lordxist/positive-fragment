#lang racket

(require (only-in "../positive.rkt" [data posdata])
         (for-syntax racket/bool))

(provide #%module-begin data)

(define-for-syntax (convert-calls stx)
  (syntax-case stx ()
    [(synt synt-elem ...)
     (and
      (symbol? (syntax->datum #'synt))
      (symbol=? 'call (syntax->datum #'synt)))
     #`(cmd #,@(map convert-calls (syntax->list #'(synt-elem ...))))]
    [(other other-elem ...) #`(other #,@(map convert-calls (syntax->list #'(other-elem ...))))]
    [other #'other]))

(define-for-syntax module-begin-def
  #`(...(define-syntax (module-begin stx)
          (syntax-case stx ()
            [(_ elem ...)
             #`(module-begin #,@(map convert-calls (syntax->list #'(elem ...))))]))))

(define-syntax (data stx)
  (syntax-case stx ()
    [(data (name ((con (type ...)) ...)) ...)
     #`(begin
         (provide (rename-out [module-begin #%module-begin]))
         (posdata #s(preprocess) (name ((con (type ...) ()) ...)) ...)
         #,module-begin-def)]))
