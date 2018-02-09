#lang racket

(provide #%module-begin data)

(require (for-syntax racket/bool
                     racket/format
                     racket/list
                     racket/match
                     racket/string))

(define-for-syntax (linear-dependencies l)
  (match l
    ['() #''()]
    [(cons x r)
     (syntax-case x ()
       [(name ((con (type ...) (cnt-type ...)) ...))
        #``(,type ... ... ,cnt-type ... ... ,(λ (name) #,(linear-dependencies r)))])]))

(define-for-syntax (constructor-defs-for-type l excluded prefix)
  (match l
    ['() #''()]
    [(cons x r)
     (syntax-case x ()
       [(con tname (type ...) (cnt-type ...))
        (let ([argheads
               (map (λ (n) (datum->syntax #f (string->symbol (string-append "sub" (~a n)))))
                    (range (length (syntax->list #'(type ...)))))]
              [args
               (map (λ (n s) #`(#,(datum->syntax #f (string->symbol (string-append "sub" (~a n))))
                                #,(make-prefab-struct (syntax->datum s))
                                #,(datum->syntax #f (string->symbol (string-append "args" (~a n))))
                                #,(datum->syntax #f (string->symbol (string-append "scargs" (~a n))))))
                    (range (length (syntax->list #'(type ...))))
                    (syntax->list #'(type ...)))]
              [cargs
               (map
                (λ (n) (datum->syntax #f (string->symbol (string-append "cargs" (~a n)))))
                (range (length (syntax->list #'(cnt-type ...)))))])
          #`(begin
              (define-syntax
                (#,(if prefix
                       (datum->syntax #f (string->symbol (string-append prefix "-" (symbol->string (syntax->datum #'con)))))
                       #'con)
                 stx)
                (syntax-case stx ()
                  [(_ #,(make-prefab-struct (syntax->datum #'tname)) (#,@args) (#,@cargs))
                   (andmap (λ (s) (and
                                   (not (string=? (symbol->string (syntax->datum s)) #,excluded))
                                   (if #,prefix
                                       (string-prefix? (symbol->string (syntax->datum s)) (string-append #,prefix "-"))
                                       (not (string-contains? (symbol->string (syntax->datum s)) "-")))))
                           (syntax->list #'(#,@argheads)))
                   #'`(#,@(map (λ (s) #`,#,s) args) #,@(map (λ (s) #`,#,s) cargs))]))
              #,(constructor-defs-for-type r excluded prefix)))])]))

(define-for-syntax (constructor-defs l excluded prefix)
  (match l
    ['() #''()]
    [(cons x r)
     (syntax-case x ()
       [(name ((con (type ...) (cnt-type ...)) ...))
        #`(begin
            #,(constructor-defs-for-type (syntax->list #'((con name (type ...) (cnt-type ...)) ...)) excluded prefix)
            #,(constructor-defs r excluded prefix))])]))

(define-for-syntax continuation-def
  #`(define-syntax (lambda stx)
      (syntax-case stx ()
        [(_ type #,#'(... (((pattern-start pattern-element ...) (cmd-start cmd-element ...)) ...)))
         #''()])))

(define-for-syntax command-def
  #`(define-syntax (cmd stx)
      (syntax-case stx ()
        [(_ cont val) #''()])))

(define-for-syntax variable-def
  #`(define-syntax (var stx)
      (syntax-case stx ()
        [(_ type n ())
         (number? (syntax->datum #'n))
         #''()])))

(define-for-syntax p-variable-def
  #`(define-syntax (p-var stx)
      (syntax-case stx ()
        [(_ type () ()) #''()])))

(define-syntax (data stx)
  (syntax-case stx ()
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (let ([slist (syntax->list #'((name ((con (type ...) (cnt-type ...)) ...)) ...))]
           [pcons (map (λ (s) (datum->syntax #f (string->symbol (string-append "p-" (symbol->string (syntax->datum s))))))
                       (syntax->list #'(con ... ...)))])
       #`(begin
           (provide #%module-begin #%app lambda cmd var p-var con ... ... #,@pcons)
           #,(linear-dependencies slist) ; disables recursive types
           #,(constructor-defs slist "lvar" #f)
           #,continuation-def
           #,command-def
           #,variable-def
           #,p-variable-def ; for variables in patterns (linear)
           #,(constructor-defs slist "var" "p")))])) ; for patterns
