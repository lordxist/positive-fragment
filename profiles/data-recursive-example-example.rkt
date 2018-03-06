#lang s-exp "data-recursive-example.rkt"

; adding two numbers, called with 4 and 2 (should produce 6)
;(call
; (lambda #s(abstr-pair-nat-unit-nat)
;   (((p-abstr-pair-nat-unit-nat #s(abstr-pair-nat-unit-nat)
;                                ((p-pair #s(pair-nat) ((p-zero #s(nat) () ()) (p-var #s(nat) () ())) ()) (tt #s(unit) () ()))
;                                ((p-var #s(nat) () ())))
;     (call (nvar #s(nat) 0 ()) (var #s(nat) 0 ())))
;    ((p-abstr-pair-nat-unit-nat #s(abstr-pair-nat-unit-nat)
;                                ((p-pair ((p-succ #s(nat) ((p-var #s(nat) () ())) ()) (p-var #s(nat) () ())) ((p-var #s(nat) () ()))) (tt #s(unit) () ())))
;     (call (rec #s(abstr-pair-nat-unit-nat) 0 ())
;           (abstr-pair-nat-unit-nat #s(abstr-pair-nat-unit-nat)
;                                    ((pair #s(pair-nat) ((var #s(nat) 0 ()) (var #s(nat) 1 ())) ()))
;                                    ((lambda #s(nat)
;                                       (((p-var #s(nat) () ()) (call (nvar #s(nat) 0 ()) (succ #s(nat) ((var #s(nat) 2 ())) ())))
;                                        (cmd daemon #s(impossible) ())))))))
;    (cmd daemon #s(impossible) ())))
; (abstr-pair-nat-unit-nat #s(abstr-pair-nat-unit-nat)
;                          ((pair #s(pair-nat)
;                                 (succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () ())) ())) ())) ())) ())
;                                 (succ #s(nat) ((succ #s(nat) ((zero #s(nat) () ())) ())) ())))
;                          ((lambda #s(nat)
;                             (((p-var #s(nat) () ()) (cmd daemon #s(print) ((var #s(nat) 0 ()))))
;                              (cmd daemon #s(impossible) ()))))))

;(lambda #s(nat) (
(call
 (lambda #s(abstr-nat-unit-nat)
   (((p-abstr-nat-unit-nat #s(abstr-nat-unit-nat) ((p-var #s(nat) () ()) (p-tt #s(unit) () ())) ((p-var #s(nat) () ()))) (cmd daemon #s(ignore) ()))
    (cmd daemon #s(impossible) ())))
 (abstr-nat-unit-nat #s(abstr-nat-unit-nat) ((zero #s(nat) () ()) (tt #s(unit) () ())) ((lambda #s(nat) ((cmd daemon #s(ignore) ()))))))
;))
