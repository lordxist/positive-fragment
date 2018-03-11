#lang racket

(require (except-in "../positive.rkt" #%module-begin)
         (for-syntax "../lib/structs.rkt"))

(provide (rename-out (module-begin #%module-begin)))

(define-syntax (module-begin stx)
  (let ([renaming #'(begin
                      (require (rename-in 'cd
                                          (lambda mu)
                                          (cmd cmdn)
                                          (var varn)
                                          (nvar nvarn)
                                          (p-var p-varn)))
                      (provide mu cmdn varn nvarn p-varn))])
    (syntax-case stx ()
      [(module-begin (data #s(recursive) (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...)
                     (codata #s(recursive) (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...) (shifts "mu" "varn"))
          (module cd "../positive.rkt"
            #,(data-helper #t #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...) (shifts "lambda" "var")))
          #,renaming
          (require (rename-in 'cd (rec recn)))
          (provide recn))]
      [(module-begin (data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...)
                     (codata (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...) (shifts "mu" "varn"))
          (module cd "../positive.rkt"
            #,(data-helper #f #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...) (shifts "lambda" "var")))
          #,renaming)]
      [(module-begin (data #s(recursive) (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...)
                     (codata (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...) (shifts "mu" "varn"))
          (module cd "../positive.rkt"
            #,(data-helper #f #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...) (shifts "lambda" "var")))
          #,renaming)]
      [(module-begin (data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...)
                     (codata #s(recursive) (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...))
       #`(#%module-begin
          #,(data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...) (shifted-type ...)) ...)) ...) (shifts "mu" "varn"))
          (module cd "../positive.rkt"
            #,(data-helper #t #f #'(codata (nname ((ncon (ntype ...) (ncnt-type ...) (nshifted-type ...)) ...)) ...) (shifts "lambda" "var")))
          #,renaming
          (require (rename-in 'cd (rec recn)))
          (provide recn))])))
