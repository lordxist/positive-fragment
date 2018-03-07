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
  (syntax-case stx ()
    [(codata #s(recursive) (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #t #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]
    [(codata (name ((con (type ...) (cnt-type ...)) ...)) ...)
     (data-helper #f #f #'(data (name ((con (type ...) (cnt-type ...)) ...)) ...))]))
