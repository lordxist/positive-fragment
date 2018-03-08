#lang s-exp "cu-example.rkt"

(succ #s(nat) ((zero #s(nat) () ())) ())

(cmd daemon #s(ignore) ())

(lambda #s(nat) ((cmd daemon #s(print) ((var #s(nat) 0 ())))))

; not allowed, since lambdas cannot consume covalues (only shifted expressions)
;(lambda #s(bot) ((cmd daemon #s(print) (fun ((var #s(bot) 0 ())) ((mu #s(bot) ((cmd daemon #s(ignore) ()))))))))

(mu #s(bot) ((cmdn daemon #s(print) (fun ((var #s(bot) 0 ())) ((mu #s(bot) ((cmd daemon #s(ignore) ()))))))))

; not allowed, since mus cannot consume values (only shifted continuations)
;(mu #s(nat) ((cmd daemon #s(print) ((var #s(nat) 0 ())))))

(cmdn daemon #s(ignore) ())
