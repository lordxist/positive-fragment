#lang s-exp "type-defs.rkt"

(require "../../cu/profiles/data-sugar.rkt")

; The code below corresponds 1:1 to the following data fragment program:
;
; function succ_f(Nat): Nat
;   succ_f(n) = succ(n)
;
; function add(Nat, Nat): Nat
;   add(zero(), n)  = n
;   add(succ(m), n) = succ_f(add(m, n))
;
; "main method" call: `add(succ(succ(zero())), succ(succ(succ(succ(zero())))))`

(primitive-print
 #s(nat)
 (primitive-apply
  (primitive-fun
   #s(pair-nat-nat) #s(nat)
   (((p-pairnatnat #s(pair-nat-nat) ((p-zero #s(nat) () () ()) (p-var #s(nat) () () ())) () ())
     (primitive-const (var #s(nat) 0 () ())))
    ((p-pairnatnat #s(pair-nat-nat) ((p-succ #s(nat) ((p-var #s(nat) () () ())) () ()) (p-var #s(nat) () () ())) () ())
     (primitive-apply
      ; corresponds to succ_f
      (primitive-fun
       #s(nat) #s(nat)
       (((p-var #s(nat) () () ()) (primitive-const (succ #s(nat) ((var #s(nat) 2 () ())) () ())))))
      ; the recursive call (TODO: write macro for encoding recursive calls)
      (mu #s(shift-nat)
          (((p-shiftnat #s(shift-nat) () () ((p-varn #s(nat) () () ())))
            (cmd (rec #s(pair-nat-nat-to-nat) 0 () ())
                 (pairnatnattonat #s(pair-nat-nat-to-nat)
                                  ((pairnatnat #s(pair-nat-nat) ((var #s(nat) 0 () ()) (var #s(nat) 1 () ())) () ()))
                                  ((nvar #s(nat) 0 () ()))
                                  ())))
           (cmdn daemon #s(impossible) ())))))))
  (primitive-const (pairnatnat #s(pair-nat-nat)
                               ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())
                                (succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())) () ())) () ()))
                               () ()))))
