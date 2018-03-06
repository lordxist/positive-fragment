#lang s-exp "positive-recursive-example.rkt"

; adding two numbers, called with 4 and 2 (should produce 6)
(cmd
 (lambda #s(nat-fun)
   (((p-fun #s(nat-fun) ((p-zero #s(nat) () ()) (p-var #s(nat) () ())) ((p-var #s(nat) () ())))
     (cmd (nvar #s(nat) 0 ()) (var #s(nat) 0 ())))
    ((p-fun #s(nat-fun) ((p-succ #s(nat) ((p-var #s(nat) () ())) ()) (p-var #s(nat) () ())) ((p-var #s(nat) () ())))
     (cmd (rec #s(nat-fun) 0 ()) (fun #s(nat-fun)
                                      ((var #s(nat) 0 ()) (var #s(nat) 1 ()))
                                      ((lambda #s(nat)
                                         (((p-var #s(nat) () ()) (cmd (nvar #s(nat) 0 ()) (succ #s(nat) ((var #s(nat) 2 ())) ())))
                                          (cmd daemon #s(impossible) ())))))))
    (cmd daemon #s(impossible) ())))
 (fun #s(nat-fun)
      ((succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () ())) ())) ())) ())) ())
       (succ #s(nat) ((succ #s(nat) ((zero #s(nat) () ())) ())) ()))
      ((lambda #s(nat)
         (((p-var #s(nat) () ()) (cmd daemon #s(print) ((var #s(nat) 0 ()))))
          (cmd daemon #s(impossible) ()))))))

; does not typecheck
; reason: the recursive call claims to reference function accepting `nat-fun` even though it really references one accepting `nat`
;(cmd
; (lambda #s(nat)
;   (((p-var #s(nat) () ()) (cmd (rec #s(nat-fun) 0 ())
;                                (fun #s(nat-fun) ((zero #s(nat) () ()) (zero #s(nat) () ())) ((lambda #s(nat) ((cmd daemon #s(ignored) ())))))))
;    (cmd daemon #s(impossible) ())))
; (zero #s(nat) () ()))
