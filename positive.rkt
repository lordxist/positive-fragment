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

(define-for-syntax (constructor-defs-for-type l prefix)
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
          #`(...
             (begin
               #,(unless prefix
                   #`(...
                      (define-syntax (con stx)
                        (syntax-case stx ()
                          [(_ type2 elem ...)
                           #'(#,(string->symbol (string-append "i-" (symbol->string (syntax->datum #'con))))
                              type2 () elem ...)]))))
               (define-syntax
                 (#,(if prefix
                        (datum->syntax #f (string->symbol (string-append prefix "-" (symbol->string (syntax->datum #'con)))))
                        (datum->syntax #f (string->symbol (string-append "i-" (symbol->string (syntax->datum #'con))))))
                  stx)
                 #,(if prefix
                       #`
                       (syntax-case stx ()
                         [(_ #,(make-prefab-struct (syntax->datum #'tname))
                             (#,@args) (#,@cargs))
                          (andmap (λ (s) (string-prefix? (symbol->string (syntax->datum s)) (string-append #,prefix "-")))
                                  (syntax->list #'(#,@argheads)))
                          #'`(#,@(map (λ (s) #`,#,s) args)
                              #,@(map (λ (s) #`,#,s) cargs)
                              )])
                       #`
                       (syntax-case stx ()
                         [(_ #,(make-prefab-struct (syntax->datum #'tname))
                             #,#'(... ((type-key binder-index) ...))
                             (#,@args) (#,@cargs))
                          (andmap (λ (s) (not (string-contains? (symbol->string (syntax->datum s)) "-")))
                                  (syntax->list #'(#,@argheads)))
                          (let ([i-args
                                 (map
                                  (λ (s)
                                    (syntax-case s ()
                                      #,#'(...
                                           [(sub type2 elem ...)
                                            #`(#,(datum->syntax s (string->symbol (string-append "i-" (symbol->string (syntax->datum #'sub)))))
                                               type2 () elem ...)])))
                                  (syntax->list #'(#,@args)))])
                            #`(list #,@i-args
                                    #,@(syntax->list #'(#,@cargs))))])))
             #,(constructor-defs-for-type r prefix))))])]))

(define-for-syntax (constructor-defs l prefix)
  (match l
    ['() #''()]
    [(cons x r)
     (syntax-case x ()
       [(name ((con (type ...) (cnt-type ...)) ...))
        #`(begin
            #,(constructor-defs-for-type (syntax->list #'((con name (type ...) (cnt-type ...)) ...)) prefix)
            #,(constructor-defs r prefix))])]))

(define-for-syntax continuation-def
  #`(...(define-syntax (lambda stx)
          (syntax-case stx ()
            [(_ type elem ...) #'(i-lambda type () elem ...)]))))

(define-for-syntax i-continuation-def
  #`(...(define-syntax (i-lambda stx)
          (syntax-case stx ()
            [(_ type
                (bound-var ...)
                (((pattern-start pattern-type pattern-element ...) (cmd-start cmd-element ...)) ...
                 (fall-through-cmd-start fall-through-cmd-element ...)))
             (and
              (andmap (λ (s) (string-prefix? (symbol->string (syntax->datum s)) "p-"))
                      (syntax->list #'(pattern-start ...)))
              (andmap (λ (s) (string=? (symbol->string (syntax->datum s)) "cmd"))
                      (syntax->list #'(cmd-start ...)))
              (string=? (symbol->string (syntax->datum #'fall-through-cmd-start)) "cmd")
              (andmap (λ (s) (symbol=? (prefab-struct-key (syntax->datum #'type))
                                       (prefab-struct-key (syntax->datum s))))
                      (syntax->list #'(pattern-type ...))))
             (letrec
                 ([prev-bound
                   (if (empty? (syntax->list #'(bound-var ...)))
                       -1
                       (string->number (last (string-split (symbol->string (syntax->datum (last (syntax->list #'(bound-var ...))))) "-"))))]
                  [new-bound
                   (λ (p)
                     (syntax-case p ()
                       [(p-start #s(bool) () ()) (symbol=? (syntax->datum #'p-start) 'p-var) 1]
                       [(p-start #s(bool) pl nl) (+ (foldl + 0 (map new-bound (syntax->list #'pl))))]))]
                  [all-bound
                   (λ (p)
                     (+ prev-bound (new-bound p)))]
                  [new-bound-vars
                   (λ (p c)
                     (map (λ (n) (datum->syntax c (string->symbol (string-append "bool-" (number->string n)))))
                          (range (+ prev-bound 1) (+ (all-bound p) 1))))]
                  [all-bound-vars
                   (λ (p c)
                     (append (syntax->list #'(bound-var ...))
                             (new-bound-vars p c)))]
                  [updated-bounds
                   (λ (p c)
                     #`(i-cmd
                        (#,@(all-bound-vars p c))
                        #,@(syntax->list
                            (syntax-case c ()
                              [(_ cmd-element2 ...) #'(cmd-element2 ...)]))))]
                  [updated-bounds-all
                   (map updated-bounds
                        (syntax->list #'((pattern-start pattern-type pattern-element ...) ...))
                        (syntax->list #'((cmd-start cmd-element ...) ...)))]
                  [binding-check
                   (λ (p c)
                     #`(λ (#,@(new-bound-vars p c)) #,(updated-bounds p c)))]
                  [binding-check-all
                   (map binding-check
                        (syntax->list #'((pattern-start pattern-type pattern-element ...) ...))
                        (syntax->list #'((cmd-start cmd-element ...) ...)))])
               ;#`(list (list (pattern-start pattern-type pattern-element ...) ...)
               ;        (list #,@binding-check-all
               ;              (fall-through-cmd-start fall-through-cmd-element ...)))
               #``(#,@(map (λ (s) #`,#,s) binding-check-all))
               )]))))

(define-for-syntax command-def
  #`(...(define-syntax (cmd stx)
          (syntax-case stx ()
            [(_ elem ...) #'(i-cmd () elem ...)]))))

(define-for-syntax i-command-def
  #`(...(define-syntax (i-cmd stx)
          (syntax-case stx ()
            [(_
              (bound-var ...)
              (cont-start cont-type cont-element ...)
              (val-start val-type val-element ...))
             (and
              (or
               (string=? (symbol->string (syntax->datum #'cont-start)) "lambda")
               (string=? (symbol->string (syntax->datum #'cont-start)) "var"))
              (not (string-prefix? (symbol->string (syntax->datum #'val-start)) "p-"))
              (not (string=? (symbol->string (syntax->datum #'val-start)) "lambda"))
              (not (string=? (symbol->string (syntax->datum #'val-start)) "cmd"))
              (symbol=? (prefab-struct-key (syntax->datum #'cont-type))
                        (prefab-struct-key (syntax->datum #'val-type))))
             #``(,(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'cont-start)))))
                   cont-type
                   (bound-var ...)
                   cont-element ...)
                 ,(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'val-start)))))
                   val-type
                   (bound-var ...)
                   val-element ...))]
            [(_ (bound-var ...) 'daemon name ((arg-start arg-type arg-element ...) ...))
             (and
              (string? (syntax->datum #'name))
              (andmap
               (λ (s) (and
                       (not (string-prefix? (symbol->string (syntax->datum s)) "p-"))
                       (not (string=? (symbol->string (syntax->datum s)) "lambda"))
                       (not (string=? (symbol->string (syntax->datum s)) "cmd"))))
               (syntax->list #'(arg-start ...))))
             #`(list
                #,@(map (λ (s)
                          (syntax-case s ()
                            [(arg-start2 arg-type2 (bound-var2 ...) arg-element2 ...)
                             #`(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'arg-start2)))))
                                arg-type2 (bound-var2 ...) arg-element2 ...)]))
                        (syntax->list
                         #'((arg-start arg-type (bound-var ...) arg-element ...) ...))))]))))

(define-for-syntax variable-def
  #`(define-syntax (var stx)
      (syntax-case stx ()
        [(_ type n ())
         (number? (syntax->datum #'n))
         #'(i-var type () n ())])))

(define-for-syntax i-variable-def
  #`(...(define-syntax (i-var stx)
          (syntax-case stx ()
            [(_ type (bound-var ...) n ())
             (number? (syntax->datum #'n))
             (if (> (syntax->datum #'n) (- (length (syntax->list #'(bound-var ...))) 1))
                 #'unbound
                 (list-ref (syntax->list #'(bound-var ...)) (syntax->datum #'n)))]))))

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
           #,(constructor-defs slist #f)
           #,continuation-def
           #,i-continuation-def ; internal representation
           #,command-def
           #,i-command-def ; internal representation
           #,variable-def
           #,i-variable-def ; internal representation (not strictly necessary, just to avoid a special case for the other grammar constructs)
           #,p-variable-def ; for variables in patterns (linear)
           #,(constructor-defs slist "p")))])) ; for patterns
