#lang s-exp "data-example.rkt"

(require "data-sugar.rkt")

(primitive-pair (mu #s(shift-nat) (((p-shiftnat #s(shift-nat) () () ((p-varn #s(nat) () () ())))
                                    (cmd (nvar #s(nat) 0 () ())
                                         (zero #s(nat) () () ())))
                                   (cmdn daemon #s(impossible) ())))
                (mu #s(shift-nat) (((p-shiftnat #s(shift-nat) () () ((p-varn #s(nat) () () ())))
                                    (cmd (nvar #s(nat) 0 () ())
                                         (zero #s(nat) () () ())))
                                   (cmdn daemon #s(impossible) ()))))
