#lang racket

(require (for-syntax racket/list
                     racket/string))

; from "On the unity of duality", p.25
(define-syntax (primitive-pair stx)
  (syntax-case stx ()
    [(_ (arg1-head arg1-type arg1-elem ...) (arg2-head arg2-type arg2-elem ...))
     (let ([unshift-type (λ (s) (string-join (rest (string-split (symbol->string (prefab-struct-key (datum->syntax s))) "-")) "-"))])
       #`(mu (((p-varn #,(make-prefab-struct (string-append "shift-pair" (unshift-type #'arg1-type) "-" (unshift-type #'arg2-type))) () () ())
               (cmdn (arg1-head arg1-type arg1-elem ...)
                     (shiftsth #s(shift-sth) () () ((lambda #s(sth) (((p-var #s(sth) () () ())
                                                                      (cmdn (arg2-head arg2-type arg2-elem ...)
                                                                            (shiftsth #s(shift-sth) () () ((lambda #s(sth) (((p-var #s(sth) () () ())
                                                                                                                             (cmdn
                                                                                                                              (nvarn #s(sth) 0 () ())
                                                                                                                              (pair #s(pair-sth)
                                                                                                                                    ((var #s(sth) 0 () ())
                                                                                                                                     (var #s(sth) 0 () ()))
                                                                                                                                    () ())))
                                                                                                                            (cmd daemon #s(impossible) ())))))))
                                                                     (cmd daemon #s(impossible) ())))))))
              (cmdn daemon #s(impossible) ()))))]))
