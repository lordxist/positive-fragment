#lang s-exp "cu-example.rkt"

; coexistence examples (no shifts)
; ================================

;(succ #s(nat) ((zero #s(nat) () () ())) () ())

;(cmd daemon #s(ignore) ())

;(lambda #s(nat) ((cmd daemon #s(print) ((var #s(nat) 0 () ())))))

; not allowed, since lambdas cannot consume covalues (only shifted expressions)
;(lambda #s(bot) ((cmd daemon #s(print) (fun ((var #s(bot) 0 () ())) ((mu #s(bot) ((cmd daemon #s(ignore) ()))))))))

;(mu #s(bot) ((cmdn daemon #s(print) ((bot-fun #s(bot-fun) ((var #s(bot) 0 () ())) ((mu #s(bot) ((cmd daemon #s(ignore) ())))) ())))))

; not allowed, since mus cannot consume values (only shifted continuations)
;(mu #s(nat) ((cmd daemon #s(print) ((var #s(nat) 0 () ())))))

;(cmdn daemon #s(ignore) ())


; shift examples
; ==============

; pseudo-boolean negation (i.e. maps 0 to 1 and n to 0 for all n > 0), called with 2
; makes use of a function type (negative) and a natural number type (positive)
(cmdn
 (mu #s(nat-fun) (((p-apply #s(nat-fun) ((p-shift #s(shift-nat) () () ((p-varn #s(nat) () () ())))) ((p-varn #s(shift-nat) () () ())) ())
                   (cmdn
                    (nvarn #s(shift-nat) 0 () ())
                    (shift #s(shift-nat) () ()
                           ((lambda #s(nat) (((p-zero #s(nat) () () ())
                                              (cmd (nvar #s(nat) 0 () ()) (succ #s(nat) ((zero #s(nat) () () ())) () ())))
                                             ((p-succ #s(nat) ((p-var #s(nat) () () ())) () ())
                                              (cmd (nvar #s(nat) 0 () ()) (zero #s(nat) () () ())))
                                             (cmd daemon #s(impossible) ())))))))
                  (cmdn daemon #s(impossible) ())))
 (apply #s(nat-fun)
        ((shift #s(shift-nat) () () ((lambda #s(nat) (((p-var #s(nat) () () ()) (cmd daemon #s(print) ((var #s(nat) 0 () ()))))
                                                      (cmd daemon #s(impossible) ()))))))
        ((mu #s(shift-nat) (((p-shift #s(shift-nat) () () ((p-varn #s(nat) () () ())))
                             (cmd (nvar #s(nat) 0 () ())
                                  (succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())))
                            (cmdn daemon #s(impossible) ()))))
        ()))

; same as the above, but called with 0
(cmdn
 (mu #s(nat-fun) (((p-apply #s(nat-fun) ((p-shift #s(shift-nat) () () ((p-varn #s(nat) () () ())))) ((p-varn #s(shift-nat) () () ())) ())
                   (cmdn
                    (nvarn #s(shift-nat) 0 () ())
                    (shift #s(shift-nat) () ()
                           ((lambda #s(nat) (((p-zero #s(nat) () () ())
                                              (cmd (nvar #s(nat) 0 () ()) (succ #s(nat) ((zero #s(nat) () () ())) () ())))
                                             ((p-succ #s(nat) ((p-var #s(nat) () () ())) () ())
                                              (cmd (nvar #s(nat) 0 () ()) (zero #s(nat) () () ())))
                                             (cmd daemon #s(impossible) ())))))))
                  (cmdn daemon #s(impossible) ())))
 (apply #s(nat-fun)
        ((shift #s(shift-nat) () () ((lambda #s(nat) (((p-var #s(nat) () () ()) (cmd daemon #s(print) ((var #s(nat) 0 () ()))))
                                                      (cmd daemon #s(impossible) ()))))))
        ((mu #s(shift-nat) (((p-shift #s(shift-nat) () () ((p-varn #s(nat) () () ())))
                             (cmd (nvar #s(nat) 0 () ())
                                  (zero #s(nat) () () ())))
                            (cmdn daemon #s(impossible) ()))))
        ()))
