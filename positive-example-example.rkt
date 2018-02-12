#lang s-exp "positive-example.rkt"

(pair #s(prod-bool) ((true #s(bool) () ()) (false #s(bool) () ())) ())

(true #s(bool) () ())

(unapply #s(subtraction-bool)
         ((true #s(bool) () ()))
         ((lambda #(bool) ((cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))))

(lambda #s(bool) ((cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))

(pair #s(prod-bool) ((var #s(bool) 0 ()) (var #s(bool) 0 ())) ())

(p-var #s(bool) () ())

(p-pair #s(prod-bool) ((p-var #s(bool) () ()) (p-var #s(bool) () ())) ())

(p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-var #s(bool) () ())) ())

(lambda #s(bool) (((p-true #s(bool) () ()) (cmd (lambda #s(bool)
                                                  ((cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))
                                                (var #s(bool) 0 ())))
                  (cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))

(cmd (var #s(bool) 0 ()) (true #s(bool) () ()))

(lambda #s(bool) ((cmd 'daemon "print" ((var #s(bool) 0 ())))))

(lambda #s(bool) (((p-var #s(bool) () ()) (cmd (lambda #s(bool)
                                                 (((p-var #s(bool) () ()) (cmd 'daemon "print" ((var #s(bool) 1 ()))))
                                                  (cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))
                                               (var #s(bool) 0 ())))
                  (cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))
