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

(mu #s(nat-fun) (((p-apply #s(nat-fun) ((p-varn #s(shift-nat) () () ())) ((p-varn #s(shift-nat) () () ())) ())
                  (cmdn
                   (nvarn #s(shift-nat) 0 () ())
                   (shift-nat #s(shift-nat) () () ((lambda #s(nat) ((cmd daemon #s(ignore) ())))))))
                 (cmdn daemon #s(impossible) ())))
