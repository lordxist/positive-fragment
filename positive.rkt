#lang racket

(provide #%module-begin data)

(require (for-syntax racket/bool
                     racket/format
                     racket/function
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
              [cargheads
               (map (λ (n) (datum->syntax #f (string->symbol (string-append "cstart" (~a n)))))
                    (range (length (syntax->list #'(cnt-type ...)))))]
              [cargs
               (map
                (λ (n s) #`(...(#,(datum->syntax #f (string->symbol (string-append "cstart" (~a n))))
                                #,(make-prefab-struct (syntax->datum s))
                                #,(datum->syntax #f (string->symbol (string-append "celem" (~a n)))) ...)))
                (range (length (syntax->list #'(cnt-type ...))))
                (syntax->list #'(cnt-type ...)))])
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
                          (and
                           (andmap (λ (s) (string-prefix? (symbol->string (syntax->datum s)) (string-append #,prefix "-")))
                                  (syntax->list #'(#,@argheads)))
                           (andmap (λ (s)
                                     (let ([name (symbol->string (syntax->datum s))])
                                       (or (string=? "lambda" name) (string=? (string-append #,prefix "-var") name))))
                                   (syntax->list #'(#,@cargheads))))
                          #'`(#,@(map (λ (s) #`,#,s) args)
                              #,@(map (λ (s) #`,#,s) cargs)
                              )])
                       #`
                       (syntax-case stx ()
                         [(_ #,(make-prefab-struct (syntax->datum #'tname))
                             #,#'(...(bound-var ...))
                             (#,@args) (#,@cargs))
                          (and
                           (andmap (λ (s) (and
                                           (not (string-contains? (symbol->string (syntax->datum s)) "-"))
                                           (not (string=? "lambda" (symbol->string (syntax->datum s))))
                                           (not (string=? "cmd" (symbol->string (syntax->datum s))))))
                                   (syntax->list #'(#,@argheads)))
                           (andmap (λ (s) (let ([name (symbol->string (syntax->datum s))])
                                       (or (string=? "lambda" name) (string=? "var" name))))
                                   (syntax->list #'(#,@cargheads))))
                          (let ([i-args
                                 (λ (arglist)
                                   (map
                                    (λ (s)
                                      (syntax-case s ()
                                        #,#'(...
                                             [(sub type2 elem ...)
                                              #`(#,(datum->syntax s (string->symbol (string-append "i-" (symbol->string (syntax->datum #'sub)))))
                                                 type2 (bound-var ...) elem ...)])))
                                    arglist))])
                            #`(list #,(make-prefab-struct (syntax->datum #'con))
                               (list #,@(i-args (syntax->list #'(#,@args))))
                               (list #,@(i-args (syntax->list #'(#,@cargs))))))])))
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

(define-for-syntax (i-continuation-def recursion?)
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
                   (λ (t)
                     (let ([prev-bound-vars-t
                            (filter (λ (s) (symbol=?
                                            t
                                            (string->symbol (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-"))))
                                    (syntax->list #'(bound-var ...)))])
                       (if (empty? prev-bound-vars-t)
                           -1
                           (string->number (last (string-split (symbol->string (syntax->datum (last prev-bound-vars-t))) "-"))))))]
                  [prev-bound-neg (λ (t) (prev-bound (string->symbol (string-append "neg-" (symbol->string t)))))]
                  [new-bound
                   (λ (t p)
                     (syntax-case p ()
                       [(p-start p-type () ())
                        (and (symbol=? t (prefab-struct-key (syntax->datum #'p-type)))
                             (symbol=? (syntax->datum #'p-start) 'p-var))
                        1]
                       [(p-start p-type pl nl) (+ (foldl + 0 (map ((curry new-bound) t) (syntax->list #'pl))))]))]
                  [new-bound-neg-helper
                   (λ (top-level?)
                     (λ (t p)
                       (syntax-case p ()
                         [(p-start p-type () ())
                          (and (symbol=? t (prefab-struct-key (syntax->datum #'p-type)))
                               (symbol=? (syntax->datum #'p-start) 'p-var))
                          (if top-level? 0 1)]
                         [(p-start p-type pl nl) (+ (foldl + 0 (map ((curry (new-bound-neg-helper #f)) t) (syntax->list #'nl))))])))]
                  [new-bound-neg (new-bound-neg-helper #t)]
                  [all-bound
                   (λ (t p)
                     (+ (prev-bound t) (new-bound t p)))]
                  [all-bound-neg
                   (λ (t p)
                     (+ (prev-bound-neg t) (new-bound-neg t p)))]
                  [new-bound-vars
                   (λ (p c t)
                     (map (λ (n) (datum->syntax c (string->symbol (string-append (symbol->string t) "-" (number->string n)))))
                          (range (+ (prev-bound t) 1) (+ (all-bound t p) 1))))]
                  [new-bound-vars-neg
                   (λ (p c t)
                     (map (λ (n) (datum->syntax c (string->symbol (string-append "neg-" (symbol->string t) "-" (number->string n)))))
                          (range (+ (prev-bound-neg t) 1) (+ (all-bound-neg t p) 1))))]
                  [bound-var-types
                   (λ (p)
                     (sort
                      (remove-duplicates
                       (syntax-case p ()
                         [(p-start p-type () ())
                          (symbol=? (syntax->datum #'p-start) 'p-var)
                          (list (prefab-struct-key (syntax->datum #'p-type)))]
                         [(p-start p-type pl nl)
                          (append-map bound-var-types (syntax->list #'pl))]))
                      symbol<?))]
                  [bound-var-types-neg-helper
                   (λ (top-level?)
                     (λ (p)
                       (sort
                        (remove-duplicates
                         (syntax-case p ()
                           [(p-start p-type () ())
                            (symbol=? (syntax->datum #'p-start) 'p-var)
                            (if top-level? empty? (list (prefab-struct-key (syntax->datum #'p-type))))]
                           [(p-start p-type pl nl)
                            (append-map (bound-var-types-neg-helper #f) (syntax->list #'nl))]))
                        symbol<?)))]
                  [bound-var-types-neg (bound-var-types-neg-helper #t)]
                  [prev-recs (prev-bound 'rec)]
                  [current-rec (datum->syntax #f (string->symbol (string-append "rec-" (number->string (+ prev-recs 1)))))]
                  [updated-bounds
                   (λ (p c)
                     #`(i-cmd
                        (#,@(append #'(bound-var ...)
                                    (append-map (((curry new-bound-vars) p) c) (bound-var-types p))
                                    (append-map (((curry new-bound-vars-neg) p) c) (bound-var-types-neg p))
                                    (if #,recursion? (list current-rec) empty)))
                        #,@(syntax->list
                            (syntax-case c ()
                              [(_ cmd-element2 ...) #'(cmd-element2 ...)]))))]
                  [binding-check
                   (λ (p c)
                     #`(λ (#,@(append-map (((curry new-bound-vars) p) c) (bound-var-types p))) #,(updated-bounds p c)))]
                  [nvar (make-hash)]
                  [match-vars (make-hash)]
                  [structify
                   (λ (global-p p)
                     (begin
                       (unless (hash-has-key? nvar global-p)
                            (begin
                              (hash-set! nvar global-p -1)
                              (hash-set! match-vars global-p empty)))
                       (syntax-case p ()
                         [(p-start p-type () ())
                          (symbol=? 'p-var (syntax->datum #'p-start))
                          (begin
                            (hash-update! nvar global-p ((curry +) 1))
                            (hash-update! match-vars global-p
                                          ((curry cons) (datum->syntax #'p-start (string->symbol
                                                                                  (string-append
                                                                                   (symbol->string (prefab-struct-key (syntax->datum #'p-type)))
                                                                                   "-" (number->string (hash-ref nvar global-p)))))))
                            (first (hash-ref match-vars global-p)))]
                         [(p-start p-type (p-element1 ...) (p-element2 ...))
                          (let ([structify-result1 (map ((curry structify) global-p) (syntax->list #'(p-element1 ...)))]
                                [structify-result2 (map ((curry structify) global-p) (syntax->list #'(p-element2 ...)))])
                            #`(list
                               `#,(make-prefab-struct (string->symbol (substring (symbol->string (syntax->datum #'p-start)) 2)))
                               (list #,@structify-result1)
                               (list #,@structify-result2)))])))]
                  [match-case
                   #`(match case
                       #,@(map (λ (p c) #`[#,(structify p p)
                                           (#,(binding-check p c)
                                            #,@(reverse (hash-ref match-vars p)))])
                               (syntax->list #'((pattern-start pattern-type pattern-element ...) ...))
                               (syntax->list #'((cmd-start cmd-element ...) ...)))
                       [_ '()])])
               #`(λ (case #,current-rec) (list #,match-case (list (pattern-start pattern-type pattern-element ...) ...))))]))))

(define-for-syntax command-def
  #`(...(define-syntax (cmd stx)
          (syntax-case stx ()
            [(_ elem ...) #'(i-cmd () elem ...)]))))

(define-for-syntax (i-command-def recursion?)
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
             (let ([cont #`(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'cont-start)))))
                            cont-type
                            (bound-var ...)
                            cont-element ...)]
                   [val #`(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'val-start)))))
                           val-type
                           (bound-var ...)
                           val-element ...)])
               #,(if recursion? #'#`(#,cont #,val #,cont) #'#`(#,cont #,val)))]
            [(_ (bound-var ...) daemon name ((arg-start arg-type arg-element ...) ...))
             (and
              (prefab-struct-key (syntax->datum #'name))
              (andmap
               (λ (s) (and
                       (not (string-prefix? (symbol->string (syntax->datum s)) "p-"))
                       (not (string=? (symbol->string (syntax->datum s)) "lambda"))
                       (not (string=? (symbol->string (syntax->datum s)) "cmd"))))
               (syntax->list #'(arg-start ...))))
             #``(#,(datum->syntax #'name (prefab-struct-key (syntax->datum #'name)))
                 ,(list
                   #,@(map (λ (s)
                             (syntax-case s ()
                               [(arg-start2 arg-type2 (bound-var2 ...) arg-element2 ...)
                                #`(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'arg-start2)))))
                                   arg-type2 (bound-var2 ...) arg-element2 ...)]))
                           (syntax->list
                            #'((arg-start arg-type (bound-var ...) arg-element ...) ...)))))]))))

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
             (let ([vars-of-type
                    (filter
                     (λ (s) (symbol=? (prefab-struct-key (syntax->datum #'type))
                                      (string->symbol (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-"))))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length vars-of-type) 1))
                   #'unbound
                   (list-ref vars-of-type (syntax->datum #'n))))]))))

(define-for-syntax nvariable-def
  #`(define-syntax (nvar stx)
      (syntax-case stx ()
        [(_ type n ())
         (number? (syntax->datum #'n))
         #'(i-nvar type () n ())])))

(define-for-syntax i-nvariable-def
  #`(...(define-syntax (i-nvar stx)
          (syntax-case stx ()
            [(_ type (bound-var ...) n ())
             (number? (syntax->datum #'n))
             (let ([vars-of-type
                    (filter
                     (λ (s) (symbol=? (prefab-struct-key (syntax->datum #'type))
                                      ; TODO: strip away the "neg-" at the start
                                      (string->symbol (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-"))))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length vars-of-type) 1))
                   #'unbound
                   (list-ref vars-of-type (syntax->datum #'n))))]))))

(define-for-syntax p-variable-def
  #`(define-syntax (p-var stx)
      (syntax-case stx ()
        [(_ type () ()) #''()])))

(define-for-syntax rec-def
  #`(...(define-syntax (rec stx)
          (syntax-case stx ()
            [(_ type () ())
             #'(i-rec type () () ())]))))

(define-for-syntax i-rec-def
  #`(...(define-syntax (i-rec stx)
          (syntax-case stx ()
            [(_ type (bound-var ...) n ())
             (number? (syntax->datum #'n))
             (let ([recs
                    (filter
                     (λ (s) (symbol=? 'rec
                                      (string->symbol (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-"))))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length recs) 1))
                   #'unbound
                   (list-ref recs (syntax->datum #'n))))]))))

(define-for-syntax (data-helper recursion? stx)
  (syntax-case stx ()
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (let ([slist (syntax->list #'((name ((con (type ...) (cnt-type ...)) ...)) ...))]
           [pcons (map (λ (s) (datum->syntax #f (string->symbol (string-append "p-" (symbol->string (syntax->datum s))))))
                       (syntax->list #'(con ... ...)))])
       #`(begin
           (provide #%app lambda cmd var p-var con ... ... #,@pcons)
           #,(unless recursion? (linear-dependencies slist)) ; disables recursive types
           #,(constructor-defs slist #f)
           #,continuation-def
           #,(i-continuation-def recursion?) ; internal representation
           #,command-def
           #,(i-command-def recursion?) ; internal representation
           #,variable-def
           #,i-variable-def ; internal representation (not strictly necessary, just to avoid a special case for the other grammar constructs)
           #,p-variable-def ; for variables in patterns (linear)
           #,(when recursion? rec-def)   ; for recursive calls
           #,(when recursion? i-rec-def) ; internal representation
           #,(constructor-defs slist "p")))])) ; for patterns

(define-syntax (data stx)
  (syntax-case stx ()
    [(data #s(recursive) #s(preprocess) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #t #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]
    [(data #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     #`(begin
         (provide #%module-begin)
         #,(data-helper #t #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...)))]
    [(data #s(preprocess) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     #`(begin
         (provide #%module-begin)
         #,(data-helper #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...)))]))
