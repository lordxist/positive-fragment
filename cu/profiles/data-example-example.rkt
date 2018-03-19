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

;(primitive-apply (lambda #s(nat-to-nat) (((p-nattonat #s(nat-to-nat)
;                                                      ((p-var #s(nat) () () ()))
;                                                      ((p-var #s(nat) () () ())) ())
;                                          (cmdn (mu #s(shift-nat)
;                                                    (((p-shiftnat #s(shift-nat) () () ((p-varn #s(nat) () () ())))
;                                                      (cmd (nvar #s(nat) 0 () ())
;                                                           (zero #s(nat) () () ())))
;                                                     (cmdn daemon #s(impossible) ())))
;                                                (shiftnat #s(shift-nat) () () ((nvar #s(nat) 0 () ())))))
;                                         (cmd daemon #s(impossible) ())))
;                 (primitive-const (zero #s(nat) () () ())))

(primitive-print
 #s(nat)
 (primitive-apply (primitive-fun #s(nat) #s(nat)
                                 (((p-var #s(nat) () () ()) (primitive-const (var #s(nat) 0 () ())))))
                  (primitive-const (zero #s(nat) () () ()))))
