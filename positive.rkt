#lang racket

(provide #%module-begin data
         (for-syntax data-helper))

(require (for-syntax racket/bool
                     racket/format
                     racket/function
                     racket/list
                     racket/match
                     racket/string
                     "lib/structs.rkt"))

(define-for-syntax (linear-dependencies l)
  (match l
    ['() #''()]
    [(cons x r)
     (syntax-case x ()
       [(name ((con (type ...) (cnt-type ...)) ...))
        #``(,type ... ... ,cnt-type ... ... ,(λ (name) #,(linear-dependencies r)))])]))

(define-for-syntax (recursive-dependencies l)
  #`(λ (#,@(map (λ (x) (syntax-case x () [(name def) #'name])) l))
      `(#,@(append-map (λ (x) (syntax-case x ()
                                [(name ((con (type ...) (cnt-type ...)) ...))
                                 (syntax->list #'(,type ... ... ,cnt-type ... ...))]))
                       l))))

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

(define-for-syntax (i-continuation-def types recursion?)
  #`(...(define-syntax (i-lambda stx)
          (syntax-case stx ()
            [(_ type
                (bound-var ...)
                (((pattern-start pattern-type pattern-element ...) (cmd-start cmd-element ...)) ...
                 (fall-through-cmd-start fall-through-cmd-element ...)))
             (and
              (member (prefab-struct-key (syntax->datum #'type)) #,types)
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
                            (if top-level? empty (list (prefab-struct-key (syntax->datum #'p-type))))]
                           [(p-start p-type pl nl)
                            (append-map (bound-var-types-neg-helper #f) (syntax->list #'nl))]))
                        symbol<?)))]
                  [bound-var-types-neg (bound-var-types-neg-helper #t)]
                  [prev-recs
                   (prev-bound (string->symbol (string-append "rec-" (symbol->string (prefab-struct-key (syntax->datum #'type))))))]
                  [current-rec (datum->syntax #f (string->symbol (string-append "rec-" (symbol->string (prefab-struct-key (syntax->datum #'type)))
                                                                                "-" (number->string (+ prev-recs 1)))))]
                  [updated-bounds
                   (λ (p c)
                     #`(i-cmd
                        (#,@(append (syntax->list #'(bound-var ...))
                                    (append-map (((curry new-bound-vars) p) c) (bound-var-types p))
                                    (append-map (((curry new-bound-vars-neg) p) c) (bound-var-types-neg p))
                                    (if #,recursion? (list current-rec) empty)))
                        #,@(syntax->list
                            (syntax-case c ()
                              [(_ cmd-element2 ...) #'(cmd-element2 ...)]))))]
                  [binding-check
                   (λ (p c)
                     #`(λ (#,@(append (append-map (((curry new-bound-vars) p) c) (bound-var-types p))
                                      (append-map (((curry new-bound-vars-neg) p) c) (bound-var-types-neg p))))
                         #,(updated-bounds p c)))]
                  [nvar (make-hash)]
                  [nvar-neg (make-hash)]
                  [match-vars (make-hash)]
                  [match-vars-neg (make-hash)]
                  [structify-helper
                   (λ (neg?)
                     (λ (global-p p)
                       (begin
                         (unless (hash-has-key? nvar global-p)
                           (begin
                             (hash-set! nvar global-p -1)
                             (hash-set! nvar-neg global-p -1)
                             (hash-set! match-vars global-p empty)
                             (hash-set! match-vars-neg global-p empty)))
                         (syntax-case p ()
                           [(p-start p-type () ())
                            (symbol=? 'p-var (syntax->datum #'p-start))
                            (begin
                              (hash-update! (if neg? nvar-neg nvar) global-p ((curry +) 1))
                              (hash-update! (if neg? match-vars-neg match-vars) global-p
                                            ((curry cons) (datum->syntax #'p-start (string->symbol
                                                                                    (string-append
                                                                                     (if neg? "neg-" "")
                                                                                     (symbol->string (prefab-struct-key (syntax->datum #'p-type)))
                                                                                     "-" (number->string (hash-ref (if neg? nvar-neg nvar) global-p)))))))
                              (first (hash-ref (if neg? match-vars-neg match-vars) global-p)))]
                           [(p-start p-type (p-element1 ...) (p-element2 ...))
                            (let ([structify-result1 (map ((curry (structify-helper #f)) global-p) (syntax->list #'(p-element1 ...)))]
                                  [structify-result2 (map ((curry (structify-helper #t)) global-p) (syntax->list #'(p-element2 ...)))])
                              #`(list
                                 `#,(make-prefab-struct (string->symbol (substring (symbol->string (syntax->datum #'p-start)) 2)))
                                 (list #,@structify-result1)
                                 (list #,@structify-result2)))]))))]
                  [structify (structify-helper #f)]
                  [match-case
                   #`(match case
                       #,@(map (λ (p c) #`[#,(structify p p)
                                           (#,(binding-check p c)
                                            #,@(append (reverse (hash-ref match-vars p)) (reverse (hash-ref match-vars-neg p))))])
                               (syntax->list #'((pattern-start pattern-type pattern-element ...) ...))
                               (syntax->list #'((cmd-start cmd-element ...) ...)))
                       [_ '()])])
               #,(if recursion?
                     #'(... #`(λ (case #,current-rec) (list #,match-case (list (pattern-start pattern-type pattern-element ...) ...))))
                     #'(... #`(λ (case) (list #,match-case (list (pattern-start pattern-type pattern-element ...) ...))))))]))))

(define-for-syntax command-def
  #`(...(define-syntax (cmd stx)
          (syntax-case stx ()
            [(_ elem ...) #'(i-cmd () elem ...)]))))

(define-for-syntax (i-command-def recursion? profile)
  #`(...(define-syntax (i-cmd stx)
          (syntax-case stx ()
            [(_
              (bound-var ...)
              (cont-start cont-type cont-element ...)
              (val-start val-type val-element ...))
             (and
              (or
               (string=? (symbol->string (syntax->datum #'cont-start)) "lambda")
               (string=? (symbol->string (syntax->datum #'cont-start)) "nvar")
               (if #,recursion? (string=? (symbol->string (syntax->datum #'cont-start)) "rec") #f))
              (not (string-prefix? (symbol->string (syntax->datum #'val-start)) "p-"))
              (not (string=? (symbol->string (syntax->datum #'val-start)) "lambda"))
              (not (string=? (symbol->string (syntax->datum #'val-start)) "cmd"))
              (symbol=? (prefab-struct-key (syntax->datum #'cont-type))
                        (prefab-struct-key (syntax->datum #'val-type))))
             (let* ([cont #`(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'cont-start)))))
                             cont-type
                             (bound-var ...)
                             cont-element ...)]
                    [val #`(#,(datum->syntax stx (string->symbol (string-append "i-" (symbol->string (syntax->datum #'val-start)))))
                            val-type
                            (bound-var ...)
                            val-element ...)]
                    [result #,(if recursion? #'#`(#,cont #,val #,cont) #'#`(#,cont #,val))])
               #,(if (and profile (profile-cont-in-cmd profile)) ; extension point for profiles
                     #`(if (or
                            (not (string=? (symbol->string (syntax->datum #'cont-start)) #,(limitation-domain (profile-cont-in-cmd profile))))
                            (string-prefix? (symbol->string (prefab-struct-key (syntax->datum #'cont-type))) #,(limitation-prefix (profile-cont-in-cmd profile))))
                           result
                           (raise-syntax-error #f #,(limitation-error-descr (profile-cont-in-cmd profile)) #'cont-start))
                     #'result))]
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
                     (λ (s) (string=? (string-append "neg-" (symbol->string (prefab-struct-key (syntax->datum #'type))))
                                      (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-")))
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
                     (λ (s) (string=? (string-append "rec-" (symbol->string (prefab-struct-key (syntax->datum #'type))))
                                      (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-")))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length recs) 1))
                   #'unbound
                   (list-ref recs (syntax->datum #'n))))]))))

(define-for-syntax (data-helper recursion? profile stx)
  (syntax-case stx ()
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (let ([slist (syntax->list #'((name ((con (type ...) (cnt-type ...)) ...)) ...))]
           [pcons (map (λ (s) (datum->syntax #f (string->symbol (string-append "p-" (symbol->string (syntax->datum s))))))
                       (syntax->list #'(con ... ...)))])
       #`(begin
           (provide #%module-begin #%app lambda cmd var p-var con ... ... #,@pcons)
           #,(if recursion?
                 (recursive-dependencies slist)
                 (linear-dependencies slist)) ; disables recursive types
           #,(constructor-defs slist #f)
           #,continuation-def
           #,(i-continuation-def #''(name ...) recursion?) ; internal representation
           #,command-def
           #,(i-command-def recursion? profile) ; internal representation
           #,variable-def
           #,i-variable-def ; internal representation (not strictly necessary, just to avoid a special case for the other grammar constructs)
           #,nvariable-def   ; for negative types
           #,i-nvariable-def ; internal representation
           #,p-variable-def ; for variables in patterns (linear)
           #,(when recursion? rec-def)   ; for recursive calls
           #,(when recursion? i-rec-def) ; internal representation
           #,(constructor-defs slist "p")))])) ; for patterns

(define-syntax (data stx)
  (syntax-case stx ()
    [(data #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]))
