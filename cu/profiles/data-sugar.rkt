#lang racket

(require (for-syntax racket/list
                     racket/string))

(provide primitive-pair)

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
            (((p-varn #,shifted-pair-type () () ())
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
