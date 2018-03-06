#lang racket

(require (only-in "../positive.rkt" [data posdata])
         (for-syntax racket/bool
                     racket/list
                     racket/match
                     racket/string))

(provide #%module-begin data)

; TODO: This does not work (since `cmd` is not in scope anymore).
; However, this "solution" would not have generalized well to a "building block" system anyway.
; Instead, there should be extension points in `positive.rkt` which let the "application profiles"
; restrict the grammar in "easy enough" ways (i.e. with very limited grammar check capabilies).
(define-for-syntax (convert-calls stx)
  (syntax-case stx ()
    [(synt-start (abstr-start type abstr-elem ...) arg)
     (and (symbol? (syntax->datum #'synt-start))
          (symbol=? 'call (syntax->datum #'synt-start))
          (symbol=? 'lambda (syntax->datum #'abstr-start)))
     (if (string-prefix? (symbol->string (prefab-struct-key (syntax->datum #'type))) "abstr-")
         #`(cmd (abstr-start type #,@(map convert-calls (syntax->list #'(abstr-elem ...)))) #,(convert-calls #'arg))
         (raise-syntax-error #f "Only synthetic abstraction types allowed (encode first-order fun.s)"))]
    [(synt-start synt-elem ...)
     (and (symbol? (syntax->datum #'synt-start))
          (symbol=? 'call (syntax->datum #'synt-start)))
     #`(cmd #,@(map convert-calls (syntax->list #'(synt-elem ...))))]
    [(synt-elem ...)
     #`(#,@(map convert-calls (syntax->list #'(synt-elem ...))))]
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
    [(data #s(recursive) (name ((con (type ...)) ...)) ...)
     #`(begin
         (provide (rename-out [module-begin #%module-begin]))
         (posdata #s(recursive) #s(preprocess)
                  (name ((con (type ...) ()) ...)) ...
                  #,@(synthetic-abstraction-types (syntax->list #'(name ...))))
         #,module-begin-def)]
    [(data (name ((con (type ...)) ...)) ...)
     #`(begin
         (provide (rename-out [module-begin #%module-begin]))
         (posdata #s(preprocess)
                  (name ((con (type ...) ()) ...)) ...
                  #,@(synthetic-abstraction-types (syntax->list #'(name ...))))
         #,module-begin-def)]))
