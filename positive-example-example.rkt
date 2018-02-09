#lang s-exp "positive-example.rkt"

(pair #s(prod-bool) ((true #s(bool) () ()) (false #s(bool) () ())) ())

(true #s(bool) () ())

(unapply #s(subtraction-bool) ((true #s(bool) () ())) ((lambda #(bool) ())))

(lambda #(bool) ())

(pair #s(prod-bool) ((var #s(bool) 0 ()) (var #s(bool) 0 ())) ())

(p-var #s(bool) () ())

(p-pair #s(prod-bool) ((p-var #s(bool) () ()) (p-var #s(bool) () ())) ())

(p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-var #s(bool) () ())) ())

(lambda #(bool) (((p-true #s(bool) () ()) (cmd (lambda #(bool) ()) (var #s(bool) 0 ())))))
