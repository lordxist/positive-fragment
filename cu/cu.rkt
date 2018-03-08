#lang racket

(require "../positive.rkt")

(provide #%module-begin data codata)

(define-syntax (data stx)
  (syntax-case stx ()
    [(data #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]
    [(data (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]))

(define-syntax (codata stx)
  (let ([renaming #'(begin
                      (require (rename-in 'cd
                                          (lambda mu)
                                          (cmd cmdn)
                                          (var varn)
                                          (p-var p-varn)))
                      (provide mu cmdn varn p-varn))])
    (syntax-case stx ()
      [(codata #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
       #`(begin
           (module cd "../positive.rkt"
             #,(data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...)))
         #,renaming)]
      [(codata (name ((con (type ...) (cnt-type ...)) ...)) ...)
       #`(begin
           (module cd racket
             #,(data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...)))
           #,renaming)])))
