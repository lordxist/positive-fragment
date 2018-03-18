#lang s-exp "data-example.rkt"

(require "data-sugar.rkt")

;(primitive-pair (mu #s(shift-nat) (((p-shiftnat #s(shift-nat) () () ((p-varn #s(nat) () () ())))
;                                    (cmd (nvar #s(nat) 0 () ())
;                                         (zero #s(nat) () () ())))
;                                   (cmdn daemon #s(impossible) ())))
;                (mu #s(shift-nat) (((p-shiftnat #s(shift-nat) () () ((p-varn #s(nat) () () ())))
;                                    (cmd (nvar #s(nat) 0 () ())
;                                         (zero #s(nat) () () ())))
;                                   (cmdn daemon #s(impossible) ()))))

(primitive-apply (lambda #s(nat-to-nat) (((p-nattonat #s(nat-to-nat)
                                                     ((p-var #s(nat) () () ()))
                                                     ((p-var #s(nat) () () ())) ())
                                          (cmd (nvar #s(nat) 0 () ())
                                               (var #s(nat) 0 () ())))
                                         (cmd daemon #s(impossible) ())))
                 (zero #s(nat) () () ()))
