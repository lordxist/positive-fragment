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
       [(name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...))
        (linear-dependencies (cons #'(name ((con (type ...) (cnt-type ...)) ...)) r))]
       [(name ((con (type ...) (cnt-type ...)) ...))
        #``(,type ... ... ,cnt-type ... ... ,(λ (name) #,(linear-dependencies r)))])]))

(define-for-syntax (recursive-dependencies l)
  #`(λ (#,@(map (λ (x) (syntax-case x () [(name def) #'name])) l))
      `(#,@(append-map (λ (x) (syntax-case x ()
                                [(name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...))
                                 (syntax->list #'(,type ... ... ,cnt-type ... ...))]
                                [(name ((con (type ...) (cnt-type ...)) ...))
                                 (syntax->list #'(,type ... ... ,cnt-type ... ...))]))
                       l))))

(define-for-syntax (constructor-def-for-type x prefix shiftinfo [disable-subshifts #f])
  (if shiftinfo
      (syntax-case x ()
        [(con tname (type ...) (cnt-type ...) (shifted-type ...))
         (let ([argheads
                (map (λ (n) (datum->syntax #f (string->symbol (string-append "sub" (~a n)))))
                     (range (length (syntax->list #'(type ...)))))]
               [args
                (map (λ (n s) #`(#,(datum->syntax #f (string->symbol (string-append "sub" (~a n))))
                                 #,(make-prefab-struct (syntax->datum s))
                                 #,(datum->syntax #f (string->symbol (string-append "args" (~a n))))
                                 #,(datum->syntax #f (string->symbol (string-append "scargs" (~a n))))
                                 #,@(if disable-subshifts empty (list (datum->syntax #f (string->symbol (string-append "sshargs" (~a n))))))))
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
                 (syntax->list #'(cnt-type ...)))]
               [shargheads
                (map (λ (n) (datum->syntax #f (string->symbol (string-append "shstart" (~a n)))))
                     (range (length (syntax->list #'(shifted-type ...)))))]
               [shargs
                (map
                 (λ (n s) #`(...(#,(datum->syntax #f (string->symbol (string-append "shstart" (~a n))))
                                 #,(make-prefab-struct (syntax->datum s))
                                 #,(datum->syntax #f (string->symbol (string-append "shelem" (~a n)))) ...)))
                 (range (length (syntax->list #'(shifted-type ...))))
                 (syntax->list #'(shifted-type ...)))])
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
                        (syntax-case stx (lambda var)
                          [(_ #,(make-prefab-struct (syntax->datum #'tname))
                              (#,@args) (#,@cargs) #,@(if disable-subshifts empty (list #`(#,@shargs))))
                           (and
                            (andmap (λ (s) (string-prefix? (symbol->string (syntax->datum s)) (string-append #,prefix "-")))
                                    (syntax->list #'(#,@argheads)))
                            (andmap (λ (s)
                                      (let ([name (symbol->string (syntax->datum s))])
                                        (or (string=? (symbol->string (syntax->datum #'lambda)) name)
                                            (string=? (string-append #,prefix "-" (symbol->string (syntax->datum #'var))) name))))
                                    (syntax->list #'(#,@cargheads)))
                            (andmap (λ (s)
                                      (let ([name (symbol->string (syntax->datum s))])
                                        (or (string=? #,(shifts-opposite-lambda shiftinfo) name)
                                            (string=? (string-append #,prefix "-" #,(shifts-opposite-var shiftinfo)) name))))
                                    (syntax->list #'(#,@shargheads))))
                           #'`(#,@(map (λ (s) #`,#,s) args)
                               #,@(map (λ (s) #`,#,s) cargs)
                               #,@(if disable-subshifts empty (map (λ (s) #`,#,s) shargs))
                               )])
                        #`
                        (syntax-case stx (lambda var)
                          [(_ #,(make-prefab-struct (syntax->datum #'tname))
                              #,#'(...(bound-var ...))
                              (#,@args) (#,@cargs) #,@(if disable-subshifts empty (list #`(#,@shargs))))
                           (and
                            (andmap (λ (s) (and
                                            (not (string-contains? (symbol->string (syntax->datum s)) "-"))
                                            (not (symbol=? (syntax->datum #'lambda) (syntax->datum s)))))
                                    (syntax->list #'(#,@argheads)))
                            (andmap (λ (s) (let ([name (symbol->string (syntax->datum s))])
                                             (or (string=? (symbol->string (syntax->datum #'lambda)) name)
                                                 (string=? (symbol->string (syntax->datum #'var)) name))))
                                    (syntax->list #'(#,@cargheads)))
                            (andmap (λ (s) (let ([name (symbol->string (syntax->datum s))])
                                             (or (string=? #,(shifts-opposite-lambda shiftinfo) name)
                                                 (string=? #,(shifts-opposite-var shiftinfo) name))))
                                    (syntax->list #'(#,@shargheads))))
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
                                     (list #,@(i-args (syntax->list #'(#,@cargs))))
                                     (list #,@(if #,disable-subshifts empty (i-args (syntax->list #'(#,@shargs)))))))]))))))])
      (syntax-case x ()
        [(con tname (type ...) (cnt-type ...))
         (constructor-def-for-type #'(con tname (type ...) (cnt-type ...) ()) prefix (shifts "" "") #t)])))

(define-for-syntax (constructor-defs-for-type l prefix shifts)
  (match l
    ['() #''()]
    [(cons x r)
     #`(begin
         #,(constructor-def-for-type x prefix shifts)
         #,(constructor-defs-for-type r prefix shifts))]))

(define-for-syntax (constructor-defs l prefix shifts)
  (match l
    ['() #''()]
    [(cons x r)
     (syntax-case x ()
       [(name ((con elem ...) ...))
        #`(begin
            #,(constructor-defs-for-type (syntax->list #'((con name elem ...) ...)) prefix shifts)
            #,(constructor-defs r prefix shifts))])]))

(define-for-syntax continuation-def
  #`(...(define-syntax (lambda stx)
          (syntax-case stx ()
            [(_ type elem ...) #'(i-lambda type () elem ...)]))))

(define-for-syntax (i-continuation-def types recursion?)
  #`(...(define-syntax (i-lambda stx)
          (syntax-case stx (cmd)
            [(_ type
                (bound-var ...)
                (((pattern-start pattern-type pattern-element ...) (cmd cmd-element ...)) ...
                 (cmd fall-through-cmd-element ...)))
             (and
              (member (prefab-struct-key (syntax->datum #'type)) #,types)
              (andmap (λ (s) (string-prefix? (symbol->string (syntax->datum s)) "p-"))
                      (syntax->list #'(pattern-start ...)))
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
                  [prev-bound-sh (λ (t) (prev-bound (string->symbol (string-append "sh-" (symbol->string t)))))]
                  [new-bound
                   (λ (t p)
                     (syntax-case p ()
                       [(p-start p-type () () ())
                        (and (symbol=? t (prefab-struct-key (syntax->datum #'p-type)))
                             (symbol=? (syntax->datum #'p-start) 'p-var))
                        1]
                       [(p-start p-type pl nl sl) (+ (foldl + 0 (map ((curry new-bound) t) (syntax->list #'pl))))]))]
                  [new-bound-neg-helper
                   (λ (top-level?)
                     (λ (t p)
                       (syntax-case p ()
                         [(p-start p-type () () ())
                          (and (symbol=? t (prefab-struct-key (syntax->datum #'p-type)))
                               (symbol=? (syntax->datum #'p-start) 'p-var))
                          (if top-level? 0 1)]
                         [(p-start p-type pl nl sl) (+ (foldl + 0 (map ((curry (new-bound-neg-helper #f)) t) (syntax->list #'nl))))])))]
                  [new-bound-neg (new-bound-neg-helper #t)]
                  [new-bound-sh-helper
                   (λ (top-level?)
                     (λ (t p)
                       (syntax-case p ()
                         [(p-start p-type () () ())
                          (and (symbol=? t (prefab-struct-key (syntax->datum #'p-type)))
                               (symbol=? (syntax->datum #'p-start) 'p-var))
                          (if top-level? 0 1)]
                         [(p-start p-type pl nl sl) (+ (foldl + 0 (map ((curry (new-bound-sh-helper #f)) t) (syntax->list #'sl))))])))]
                  [new-bound-sh (new-bound-sh-helper #t)]
                  [all-bound
                   (λ (t p)
                     (+ (prev-bound t) (new-bound t p)))]
                  [all-bound-neg
                   (λ (t p)
                     (+ (prev-bound-neg t) (new-bound-neg t p)))]
                  [all-bound-sh
                   (λ (t p)
                     (+ (prev-bound-sh t) (new-bound-sh t p)))]
                  [new-bound-vars
                   (λ (p c t)
                     (map (λ (n) (datum->syntax c (string->symbol (string-append (symbol->string t) "-" (number->string n)))))
                          (range (+ (prev-bound t) 1) (+ (all-bound t p) 1))))]
                  [new-bound-vars-neg
                   (λ (p c t)
                     (map (λ (n) (datum->syntax c (string->symbol (string-append "neg-" (symbol->string t) "-" (number->string n)))))
                          (range (+ (prev-bound-neg t) 1) (+ (all-bound-neg t p) 1))))]
                  [new-bound-vars-sh
                   (λ (p c t)
                     (map (λ (n) (datum->syntax c (string->symbol (string-append "sh-" (symbol->string t) "-" (number->string n)))))
                          (range (+ (prev-bound-sh t) 1) (+ (all-bound-sh t p) 1))))]
                  [bound-var-types
                   (λ (p)
                     (sort
                      (remove-duplicates
                       (syntax-case p ()
                         [(p-start p-type () () ())
                          (symbol=? (syntax->datum #'p-start) 'p-var)
                          (list (prefab-struct-key (syntax->datum #'p-type)))]
                         [(p-start p-type pl nl sl)
                          (append-map bound-var-types (syntax->list #'pl))]))
                      symbol<?))]
                  [bound-var-types-neg-helper
                   (λ (top-level?)
                     (λ (p)
                       (sort
                        (remove-duplicates
                         (syntax-case p ()
                           [(p-start p-type () () ())
                            (symbol=? (syntax->datum #'p-start) 'p-var)
                            (if top-level? empty (list (prefab-struct-key (syntax->datum #'p-type))))]
                           [(p-start p-type pl nl sl)
                            (append-map (bound-var-types-neg-helper #f) (syntax->list #'nl))]))
                        symbol<?)))]
                  [bound-var-types-neg (bound-var-types-neg-helper #t)]
                  [bound-var-types-sh-helper
                   (λ (top-level?)
                     (λ (p)
                       (sort
                        (remove-duplicates
                         (syntax-case p ()
                           [(p-start p-type () () ())
                            (symbol=? (syntax->datum #'p-start) 'p-var)
                            (if top-level? empty (list (prefab-struct-key (syntax->datum #'p-type))))]
                           [(p-start p-type pl nl sl)
                            (append-map (bound-var-types-sh-helper #f) (syntax->list #'nl))]))
                        symbol<?)))]
                  [bound-var-types-sh (bound-var-types-sh-helper #t)]
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
                                    (append-map (((curry new-bound-vars-sh) p) c) (bound-var-types-sh p))
                                    (if #,recursion? (list current-rec) empty)))
                        #,@(syntax->list
                            (syntax-case c ()
                              [(_ cmd-element2 ...) #'(cmd-element2 ...)]))))]
                  [binding-check
                   (λ (p c)
                     #`(λ (#,@(append (append-map (((curry new-bound-vars) p) c) (bound-var-types p))
                                      (append-map (((curry new-bound-vars-neg) p) c) (bound-var-types-neg p))
                                      (append-map (((curry new-bound-vars-sh) p) c) (bound-var-types-sh p))))
                         #,(updated-bounds p c)))]
                  [nvar (make-hash)]
                  [nvar-neg (make-hash)]
                  [nvar-sh (make-hash)]
                  [match-vars (make-hash)]
                  [match-vars-neg (make-hash)]
                  [match-vars-sh (make-hash)]
                  [structify-helper
                   (λ (mode)
                     (λ (global-p p)
                       (begin
                         (unless (hash-has-key? nvar global-p)
                           (begin
                             (hash-set! nvar global-p -1)
                             (hash-set! nvar-neg global-p -1)
                             (hash-set! nvar-sh global-p -1)
                             (hash-set! match-vars global-p empty)
                             (hash-set! match-vars-neg global-p empty)
                             (hash-set! match-vars-sh global-p empty)))
                         (syntax-case p ()
                           [(p-start p-type () () ())
                            (symbol=? 'p-var (syntax->datum #'p-start))
                            (begin
                              (hash-update! (cond [(symbol=? 'sh mode) nvar-sh]
                                                  [(symbol=? 'neg mode) nvar-neg]
                                                  [else nvar])
                                            global-p ((curry +) 1))
                              (hash-update! (cond [(symbol=? 'sh mode) match-vars-sh]
                                                  [(symbol=? 'neg mode) match-vars-neg]
                                                  [else match-vars])
                                            global-p
                                            ((curry cons) (datum->syntax #'p-start (string->symbol
                                                                                    (string-append
                                                                                     (cond [(symbol=? 'sh mode) "sh-"]
                                                                                           [(symbol=? 'neg mode) "neg-"]
                                                                                           [else ""])
                                                                                     (symbol->string (prefab-struct-key (syntax->datum #'p-type)))
                                                                                     "-" (number->string (hash-ref (cond [(symbol=? 'sh mode) nvar-sh]
                                                                                                                         [(symbol=? 'neg mode) nvar-neg]
                                                                                                                         [else nvar])
                                                                                                                   global-p)))))))
                              (first (hash-ref (cond [(symbol=? 'sh mode) nvar-sh]
                                                     [(symbol=? 'neg mode) nvar-neg]
                                                     [else nvar])
                                               global-p)))]
                           [(p-start p-type (p-element1 ...) (p-element2 ...) (p-element3 ...))
                            (let ([structify-result1 (map ((curry (structify-helper 'normal)) global-p) (syntax->list #'(p-element1 ...)))]
                                  [structify-result2 (map ((curry (structify-helper 'neg)) global-p) (syntax->list #'(p-element2 ...)))]
                                  [structify-result3 (map ((curry (structify-helper 'sh)) global-p) (syntax->list #'(p-element3 ...)))])
                              #`(list
                                 `#,(make-prefab-struct (string->symbol (substring (symbol->string (syntax->datum #'p-start)) 2)))
                                 (list #,@structify-result1)
                                 (list #,@structify-result2)
                                 (list #,@structify-result3)))]))))]
                  [structify (structify-helper #f)]
                  [match-case
                   #`(match case
                       #,@(map (λ (p c) #`[#,(structify p p)
                                           (#,(binding-check p c)
                                            #,@(append (reverse (hash-ref match-vars p)) (reverse (hash-ref match-vars-neg p))))])
                               (map
                                (λ (s)
                                  (syntax-case s ()
                                    [(p-start p-type p-elem1 p-elem2)
                                     #'(p-start p-type p-elem1 p-elem2 ())]
                                    [other
                                     #'other]))
                                (syntax->list #'((pattern-start pattern-type pattern-element ...) ...)))
                               (syntax->list #'((cmd-start cmd-element ...) ...)))
                       [_ '()])])
               #,(if recursion?
                     #'(... #`(λ (case #,current-rec) (list #,match-case (list (pattern-start pattern-type pattern-element ...) ...))))
                     #'(... #`(λ (case) (list #,match-case (list (pattern-start pattern-type pattern-element ...) ...))))))]))))

(define-for-syntax command-def
  #`(...(define-syntax (cmd stx)
          (syntax-case stx (i-cmd)
            [(_ elem ...) #`(i-cmd () elem ...)]))))

(define-for-syntax (i-command-def recursion? profile)
  #`(...(define-syntax (i-cmd stx)
          (syntax-case stx (lambda nvar cmd rec)
            [(_
              (bound-var ...)
              (cont-start cont-type cont-element ...)
              (val-start val-type val-element ...))
             (and
              (or
               (symbol=? (syntax->datum #'cont-start) (syntax->datum #'lambda))
               (symbol=? (syntax->datum #'cont-start) (syntax->datum #'nvar))
               (if #,recursion? (symbol=? (syntax->datum #'cont-start) (syntax->datum #'rec)) #f))
              (not (string-prefix? (symbol->string (syntax->datum #'val-start)) "p-"))
              (not (symbol=? (syntax->datum #'val-start) (syntax->datum #'lambda)))
              (prefab-struct-key (syntax->datum #'val-type))
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

(define-for-syntax (variable-def shifts?)
  #`(define-syntax (var stx)
      (syntax-case stx ()
        [(_ type n () #,@(if shifts? (list #'()) empty))
         (number? (syntax->datum #'n))
         #'(i-var type () n ())])))

(define-for-syntax (i-variable-def shifts)
  #`(...(define-syntax (i-var stx)
          (syntax-case stx ()
            [(_ type (bound-var ...) n () #,@(if shifts? (list #'()) empty))
             (number? (syntax->datum #'n))
             (let ([vars-of-type
                    (filter
                     (λ (s) (symbol=? (prefab-struct-key (syntax->datum #'type))
                                      (string->symbol (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-"))))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length vars-of-type) 1))
                   #'unbound
                   (list-ref vars-of-type (syntax->datum #'n))))]))))

(define-for-syntax (nvariable-def shifts?)
  #`(define-syntax (nvar stx)
      (syntax-case stx ()
        [(_ type n () #,@(if shifts? (list #'()) empty))
         (number? (syntax->datum #'n))
         #'(i-nvar type () n ())])))

(define-for-syntax (i-nvariable-def shifts?)
  #`(...(define-syntax (i-nvar stx)
          (syntax-case stx ()
            [(_ type (bound-var ...) n () #,@(if shifts? (list #'()) empty))
             (number? (syntax->datum #'n))
             (let ([vars-of-type
                    (filter
                     (λ (s) (string=? (string-append "neg-" (symbol->string (prefab-struct-key (syntax->datum #'type))))
                                      (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-")))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length vars-of-type) 1))
                   #'unbound
                   (list-ref vars-of-type (syntax->datum #'n))))]))))

(define-for-syntax (p-variable-def shifts?)
  #`(define-syntax (p-var stx)
      (syntax-case stx ()
        [(_ type () () #,@(if shifts? (list #'()) empty)) #''()])))

(define-for-syntax (rec-def shifts?)
  #`(...(define-syntax (rec stx)
          (syntax-case stx ()
            [(_ type () () #,@(if shifts? (list #'()) empty))
             #'(i-rec type () () ())]))))

(define-for-syntax (i-rec-def shifts?)
  #`(...(define-syntax (i-rec stx)
          (syntax-case stx ()
            [(_ type (bound-var ...) n () #,@(if shifts? (list #'()) empty))
             (number? (syntax->datum #'n))
             (let ([recs
                    (filter
                     (λ (s) (string=? (string-append "rec-" (symbol->string (prefab-struct-key (syntax->datum #'type))))
                                      (string-join (reverse (rest (reverse (string-split (symbol->string (syntax->datum s)) "-")))) "-")))
                     (syntax->list #'(bound-var ...)))])
               (if (> (syntax->datum #'n) (- (length recs) 1))
                   #'unbound
                   (list-ref recs (syntax->datum #'n))))]))))

(define-for-syntax (data-helper recursion? profile provide-internals stx [shifts #f])
  (syntax-case stx ()
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper recursion? profile #'(data (name ((con (type ...) (cnt-type ...) ()) ...)) ...) #f)]
    [(data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...)
     (let ([slist (syntax->list (if shifts
                                    #'((name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...)
                                    #'((name ((con (type ...) (cnt-type ...)) ...)) ...)))]
           [pcons (map (λ (s) (datum->syntax #f (string->symbol (string-append "p-" (symbol->string (syntax->datum s))))))
                       (syntax->list #'(con ... ...)))])
       #`(begin
           (provide #%module-begin #%app lambda cmd var nvar p-var con ... ... #,@pcons)
           #,(when recursion? #'(provide rec))
           #,(when provide-internals #'(provide i-cmd))
           #,(if recursion?
                 (recursive-dependencies slist)
                 (linear-dependencies slist)) ; disables recursive types
           #,(constructor-defs slist #f shifts)
           #,continuation-def
           #,(i-continuation-def #''(name ...) recursion?) ; internal representation
           #,command-def
           #,(i-command-def recursion? profile) ; internal representation
           #,(variable-def shifts)
           #,(i-variable-def shifts) ; internal representation (not strictly necessary, just to avoid a special case for the other grammar constructs)
           #,(nvariable-def shifts)   ; for negative types
           #,(i-nvariable-def shifts) ; internal representation
           #,(p-variable-def shifts) ; for variables in patterns (linear)
           #,(when recursion? (rec-def shifts))   ; for recursive calls
           #,(when recursion? (i-rec-def shifts)) ; internal representation
           #,(constructor-defs slist "p" shifts)))])) ; for patterns

(define-syntax (data stx)
  (syntax-case stx ()
    [(data #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #t #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #f #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]))
