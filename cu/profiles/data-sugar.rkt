#lang racket

(require (for-syntax racket/list
                     racket/string))

(provide primitive-apply primitive-pair)

(define-syntax (primitive-apply stx)
  (syntax-case stx ()
    [(p (cont-head cont-type cont-elem ...) (arg-head arg-type arg-elem ...))
     (let* ([in-type (string-join (rest (string-split (symbol->string (prefab-struct-key (syntax->datum #'arg-type))) "-")) "-")]
            [out-type (last (string-split (symbol->string (prefab-struct-key (syntax->datum #'cont-type))) "-to-"))])
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

              (cmdn (arg-head arg-type arg-elem ...)
                    (#,(datum->syntax #f (string->symbol (string-replace (string-append "shift" in-type) "-" "")))
                     #,(make-prefab-struct (string->symbol (string-append "shift-" in-type)))
                     () ()
                     ((lambda #,(make-prefab-struct (string->symbol in-type))
                        (((p-var #,(make-prefab-struct (string->symbol in-type)) () () ())
                          (cmd (cont-head cont-type cont-elem ...)
                               (#,(datum->syntax #f (string->symbol (string-replace (string-append in-type "to" out-type) "-" "")))
                                #,(make-prefab-struct (string->symbol (string-append in-type "-to-" out-type)))
                                ((var #,(make-prefab-struct (string->symbol in-type)) 0 () ()))
                                ((nvar #,(make-prefab-struct (string->symbol out-type)) 0 () ()))
                                ())))
                         (cmd daemon #s(impossible) ())))))))
             (cmdn daemon #s(impossible) ()))))))]))

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
