#lang racket

(require (except-in "../positive.rkt" #%module-begin))

(provide (rename-out (module-begin #%module-begin)))

(define-syntax (module-begin stx)
  (let ([renaming #'(begin
                      (require (rename-in 'cd
                                          (lambda mu)
                                          (cmd cmdn)
                                          (var varn)
                                          (p-var p-varn)))
                      (provide mu cmdn varn p-varn))])
    (syntax-case stx ()
      [(module-begin (data #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
                     (codata #s(recursive) (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))
          (module cd "../positive.rkt"
            #,(data-helper #t #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...)))
          #,renaming)]
      [(module-begin (data (name ((con (type ...) (cnt-type ...)) ...)) ...)
                     (codata (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))
          (module cd "../positive.rkt"
            #,(data-helper #f #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...)))
          #,renaming)]
      [(module-begin (data #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
                     (codata (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))
          (module cd "../positive.rkt"
            #,(data-helper #f #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...)))
          #,renaming)]
      [(module-begin (data (name ((con (type ...) (cnt-type ...)) ...)) ...)
                     (codata #s(recursive) (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))
          (module cd "../positive.rkt"
            #,(data-helper #t #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...)) ...)) ...)))
          #,renaming)])))
