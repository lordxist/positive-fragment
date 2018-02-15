#lang s-exp "positive-example.rkt"
;
;(pair #s(prod-bool) ((true #s(bool) () ()) (false #s(bool) () ())) ())
;
;(true #s(bool) () ())
;
;(unapply #s(subtraction-bool)
;         ((true #s(bool) () ()))
;         ((lambda #s(bool) ((cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))))
;
;(lambda #s(bool) ((cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))
;
;(pair #s(prod-bool) ((var #s(bool) 0 ()) (var #s(bool) 0 ())) ())
;
;(p-var #s(bool) () ())
;
;(p-pair #s(prod-bool) ((p-var #s(bool) () ()) (p-var #s(bool) () ())) ())
;
;(p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-var #s(bool) () ())) ())
;
;(lambda #s(bool) (((p-true #s(bool) () ()) (cmd (lambda #s(bool)
;                                                  ((cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))
;                                                (var #s(bool) 0 ())))
;                  (cmd (var #s(bool) 0 ()) (true #s(bool) () ()))))
;
;(cmd (var #s(bool) 0 ()) (true #s(bool) () ()))
;
;(lambda #s(bool) ((cmd 'daemon "print" ((var #s(bool) 0 ())))))
;
;(lambda #s(bool) (((p-var #s(bool) () ()) (cmd (lambda #s(bool)
;                                                 (((p-var #s(bool) () ()) (cmd 'daemon "print" ((var #s(bool) 1 ()))))
;                                                  (cmd 'daemon "print" ((var #s(bool) 0 ())))))
;                                               (var #s(bool) 0 ())))
;                  (cmd 'daemon "print" ((var #s(bool) 0 ())))))
;
;(unapply #s(subtraction-bool) ((true #s(bool) () ())) ((lambda #s(bool) ((cmd 'daemon "print" ((var #s(bool) 0 ())))))))
;
;(cmd (lambda #s(bool)
;       (((p-true #s(bool) () ())  (cmd daemon #s(print) ((false #s(bool) () ()))))
;        ((p-var #s(bool) () ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
;       (cmd 'daemon #s(impossible) ())))
;     (true #s(bool) () ()))

; boolean implication (directly "print"ed)
; ========================================

; 1. invoked with (true, true)
(cmd (lambda #s(prod-bool)
       (((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-false #s(bool) () ())) ()) (cmd daemon #s(print) ((false #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-true #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-false #s(bool) () ()) (p-var #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        (cmd daemon #s(impossible) ())))
     (pair #s(prod-bool) ((true #s(bool) () ()) (true #s(bool) () ())) ()))

; 2. invoked with (true, false)
(cmd (lambda #s(prod-bool)
       (((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-false #s(bool) () ())) ()) (cmd daemon #s(print) ((false #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-true #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-false #s(bool) () ()) (p-var #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        (cmd daemon #s(impossible) ())))
     (pair #s(prod-bool) ((true #s(bool) () ()) (false #s(bool) () ())) ()))

; 3. invoked with (false, true)
(cmd (lambda #s(prod-bool)
       (((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-false #s(bool) () ())) ()) (cmd daemon #s(print) ((false #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-true #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-false #s(bool) () ()) (p-var #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        (cmd daemon #s(impossible) ())))
     (pair #s(prod-bool) ((false #s(bool) () ()) (true #s(bool) () ())) ()))

; 4. invoked with (false, false)
(cmd (lambda #s(prod-bool)
       (((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-false #s(bool) () ())) ()) (cmd daemon #s(print) ((false #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-true #s(bool) () ()) (p-true #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        ((p-pair #s(prod-bool) ((p-false #s(bool) () ()) (p-var #s(bool) () ())) ()) (cmd daemon #s(print) ((true #s(bool) () ()))))
        (cmd daemon #s(impossible) ())))
     (pair #s(prod-bool) ((false #s(bool) () ()) (false #s(bool) () ())) ()))
