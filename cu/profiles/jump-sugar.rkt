#lang racket

(require (for-syntax racket/bool
                     racket/list
                     racket/string))

(provide primitive-reccall primitive-print primitive-const primitive-fun primitive-return primitive-apply primitive-pair)

(define-for-syntax (primitive-reccall-expand stx)
  (syntax-case stx ()
    [(p in-type out-type index arg)
     (datum->syntax
      #'p
      (syntax->datum
       #`(mu #,(make-prefab-struct (string->symbol (string-append "shift-" (symbol->string (prefab-struct-key (syntax->datum #'out-type))))))
             (((#,(datum->syntax #f (string->symbol (string-append "p-shift"
                                                                   (string-replace (symbol->string (prefab-struct-key (syntax->datum #'out-type))) "-" ""))))
                #,(make-prefab-struct (string->symbol (string-append "shift-" (symbol->string (prefab-struct-key (syntax->datum #'out-type))))))
                () ()
                ((p-varn out-type () () ())))
               (cmd (rec #,(make-prefab-struct (string->symbol (string-append (symbol->string (prefab-struct-key (syntax->datum #'in-type))) "-to-"
                                                                              (symbol->string (prefab-struct-key (syntax->datum #'out-type))))))
                      index () ())
                    (#,(datum->syntax #f (string->symbol (string-replace
                                                          (string-append (symbol->string (prefab-struct-key (syntax->datum #'in-type))) "to"
                                                                         (symbol->string (prefab-struct-key (syntax->datum #'out-type))))
                                                          "-" "")))
                     #,(make-prefab-struct (string->symbol (string-append (symbol->string (prefab-struct-key (syntax->datum #'in-type))) "-to-"
                                                                          (symbol->string (prefab-struct-key (syntax->datum #'out-type))))))
                     
                     (arg)
                     ((nvar out-type 0 () ()))
                     ())))
              (cmdn daemon #s(impossible) ())))))]))

(define-syntax (primitive-reccall stx)
  (primitive-reccall-expand stx))

(define-syntax (primitive-print stx)
  (syntax-case stx ()
    [(p type expr)
     (datum->syntax
      #'p
      (syntax->datum
       #`(cmdn
          #,(syntax-case #'expr ()
              [(expr-head expr-elem ...)
               (symbol=? 'primitive-const (syntax->datum #'expr-head))
               (primitive-const-expand #'expr)]
              [(expr-head expr-elem ...)
               (symbol=? 'primitive-apply (syntax->datum #'expr-head))
               (primitive-apply-expand #'expr)]
              [_ #'case-expr])
          (#,(datum->syntax #f (string->symbol (string-append "shift" (string-replace (symbol->string (prefab-struct-key (syntax->datum #'type))) "-" ""))))
           #,(make-prefab-struct (string->symbol (string-append "shift-" (symbol->string (prefab-struct-key (syntax->datum #'type))))))
           () ()
           ((lambda type
              (((p-var type () () ()) (cmd daemon #s(print) ((var type 0 () ()))))
               (cmd daemon #s(impossible) ()))))))))]))

(define-for-syntax (primitive-const-expand stx)
  (syntax-case stx ()
    [(p (val-start val-type val-elem ...))
     (datum->syntax
      #'p
      (syntax->datum
       #`(mu #,(make-prefab-struct (string->symbol (string-append  "shift-" (symbol->string (prefab-struct-key (syntax->datum #'val-type))))))
             (((#,(datum->syntax #f (string->symbol (string-append  "p-shift" (string-replace (symbol->string (prefab-struct-key (syntax->datum #'val-type))) "-" ""))))
                #,(make-prefab-struct (string->symbol (string-append  "shift-" (symbol->string (prefab-struct-key (syntax->datum #'val-type))))))
                () ()
                ((p-varn val-type () () ())))
               (cmd (nvar val-type 0 () ())
                    (val-start val-type val-elem ...)))
              (cmdn daemon #s(impossible) ())))))]))

(define-syntax (primitive-const stx)
  (primitive-const-expand stx))

(define-for-syntax (primitive-fun-expand stx)
  (syntax-case stx ()
    [(p in-type out-type ((pattern case-expr) ...))
     (let* ([fun-type
             (string-append (symbol->string (prefab-struct-key (syntax->datum #'in-type))) "-to-"
                            (symbol->string (prefab-struct-key (syntax->datum #'out-type))))]
            [shift-out-type
             (string-append "shift-" (symbol->string (prefab-struct-key (syntax->datum #'out-type))))])
       (datum->syntax
        #'p
        (syntax->datum
         #`(lambda #,(make-prefab-struct (string->symbol fun-type))
             (#,@(map (λ (s)
                        (syntax-case s ()
                          [(pattern case-expr)
                           #`((#,(datum->syntax #f (string->symbol (string-append "p-" (string-replace fun-type "-" ""))))
                               #,(make-prefab-struct (string->symbol fun-type))
                               (pattern)
                               ((p-var out-type () () ()))
                               ())
                              #,(primitive-return-expand #'case-expr))]))
                      (syntax->list #'((pattern case-expr) ...)))
              (cmd daemon #s(impossible) ()))))))]))

(define-syntax (primitive-fun stx)
  (primitive-fun-expand stx))

(define-for-syntax (primitive-return-expand stx)
  (syntax-case stx ()
    [(p cont-index out-type case-expr)
     (let ([shift-out-type
            (string-append "shift-" (symbol->string (prefab-struct-key (syntax->datum #'out-type))))])
       (datum->syntax
        #'p
        (syntax->datum
         #`(cmdn #,(syntax-case #'case-expr ()
                     [(expr-head expr-elem ...)
                      (symbol=? 'primitive-const (syntax->datum #'expr-head))
                      (primitive-const-expand #'case-expr)]
                     [(expr-head expr-elem ...)
                      (symbol=? 'primitive-apply (syntax->datum #'expr-head))
                      (primitive-apply-expand #'case-expr)]
                     [(expr-head expr-elem ...)
                      (symbol=? 'primitive-reccall (syntax->datum #'expr-head))
                      (primitive-reccall-expand #'case-expr)]
                     [_ #'case-expr])
                 (#,(datum->syntax #f (string->symbol (string-replace shift-out-type "-" "")))
                  #,(make-prefab-struct (string->symbol shift-out-type))
                  () () ((nvar out-type cont-index () ())))))))]))

(define-syntax (primitive-return stx)
  (primitive-return-expand stx))

(define-for-syntax (primitive-apply-expand stx)
  (syntax-case stx ()
    [(p (fun-head fun-in-type fun-out-type fun-elem ...) (arg-head arg-elem ...))
     (let ([in-type (symbol->string (prefab-struct-key (syntax->datum #'fun-in-type)))]
           [out-type (symbol->string (prefab-struct-key (syntax->datum #'fun-out-type)))])
       (datum->syntax
        #'p
        (syntax->datum
         #`(mu
            #,(make-prefab-struct (string->symbol (string-append "shift-" out-type)))
            (((#,(datum->syntax #f (string->symbol
                                    (string-append "p-"
                                                   (string-replace (string-append "shift" out-type) "-" ""))))
               #,(make-prefab-struct (string->symbol (string-append "shift-" out-type)))
               () ()
               ((p-varn #,(make-prefab-struct (string->symbol out-type)) () () ())))
              (cmdn #,(cond [(symbol=? (syntax->datum #'arg-head) 'primitive-const)
                             (primitive-const-expand #'(arg-head arg-elem ...))]
                            [(symbol=? (syntax->datum #'arg-head) 'primitive-reccall)
                             (primitive-reccall-expand #'(arg-head arg-elem ...))]
                            [(symbol=? (syntax->datum #'arg-head) 'primitive-apply)
                             (primitive-apply-expand #'(arg-head arg-elem ...))]
                            [else #'(arg-head arg-elem ...)])
                    (#,(datum->syntax #f (string->symbol (string-replace (string-append "shift" in-type) "-" "")))
                     #,(make-prefab-struct (string->symbol (string-append "shift-" in-type)))
                     () ()
                     ((lambda #,(make-prefab-struct (string->symbol in-type))
                        (((p-var #,(make-prefab-struct (string->symbol in-type)) () () ())
                          (cmd #,(primitive-fun-expand #'(fun-head fun-in-type fun-out-type fun-elem ...))
                               (#,(datum->syntax #f (string->symbol (string-replace (string-append in-type "to" out-type) "-" "")))
                                #,(make-prefab-struct (string->symbol (string-append in-type "-to-" out-type)))
                                ((var #,(make-prefab-struct (string->symbol in-type)) 0 () ()))
                                ((nvar #,(make-prefab-struct (string->symbol out-type)) 0 () ()))
                                ())))
                         (cmd daemon #s(impossible) ())))))))
             (cmdn daemon #s(impossible) ()))))))]))

(define-syntax (primitive-apply stx)
  (primitive-apply-expand stx))

; from "On the unity of duality", p.25
(define-syntax (primitive-pair stx)
  (syntax-case stx ()
    [(p (arg1-head arg1-type arg1-elem ...) (arg2-head arg2-type arg2-elem ...))
     (let* ([unshift-type (λ (s) (string-join (rest (string-split (symbol->string (prefab-struct-key (syntax->datum s))) "-")) "-"))]
            [shifted-pair-type (make-prefab-struct (string->symbol (string-append "shift-pair-" (unshift-type #'arg1-type) "-" (unshift-type #'arg2-type))))])
       (datum->syntax
        #'p
        (syntax->datum
         #`(mu
            #,shifted-pair-type
            (((#,(datum->syntax #f (string->symbol (string-append "p-shiftpair" (unshift-type #'arg1-type) (unshift-type #'arg2-type))))
               #,shifted-pair-type () ()
               ((p-varn #,(make-prefab-struct (string->symbol (string-append "pair-" (unshift-type #'arg1-type) "-" (unshift-type #'arg2-type)))) () () ())))
              (cmdn
               (arg1-head arg1-type arg1-elem ...)
               (#,(datum->syntax #f (string->symbol (string-append "shift" (string-replace (unshift-type #'arg1-type) "-" ""))))
                arg1-type
                () ()
                ((lambda #,(make-prefab-struct (string->symbol (unshift-type #'arg1-type)))
                   (((p-var #,(make-prefab-struct (string->symbol (unshift-type #'arg1-type))) () () ())
                     (cmdn (arg2-head arg2-type arg2-elem ...)
                           (#,(datum->syntax #f (string->symbol (string-append "shift" (string-replace (unshift-type #'arg2-type) "-" ""))))
                            arg2-type
                            () ()
                            ((lambda #,(make-prefab-struct (string->symbol (unshift-type #'arg2-type)))
                               (((p-var #,(make-prefab-struct (string->symbol (unshift-type #'arg2-type))) () () ())
                                 (cmd
                                  (nvar #,(make-prefab-struct (string->symbol (string-append "pair-" (unshift-type #'arg1-type) "-" (unshift-type #'arg2-type)))) 0 () ())
                                  (#,(datum->syntax #f (string->symbol (string-append "pair"
                                                                                      (string-replace
                                                                                       (string-append (unshift-type #'arg1-type) "-" (unshift-type #'arg2-type))
                                                                                       "-" ""))))
                                   #,(make-prefab-struct (string->symbol (string-append "pair-" (unshift-type #'arg2-type) "-" (unshift-type #'arg2-type))))
                                   ((var #,(make-prefab-struct (string->symbol (unshift-type #'arg1-type))) 0 () ())
                                    (var #,(make-prefab-struct (string->symbol (unshift-type #'arg2-type))) 0 () ()))
                                   () ())))
                                (cmd daemon #s(impossible) ())))))))
                    (cmd daemon #s(impossible) ())))))))
             (cmd daemon #s(impossible) ()))))))]))
