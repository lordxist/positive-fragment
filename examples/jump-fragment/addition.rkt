#lang s-exp "type-defs.rkt"

(require "../../cu/profiles/jump-sugar.rkt")

; The code below corresponds 1:1 to the following program in the data fragment enhanced
; with callcc.
;
; function succ_f(Nat): Nat
;   succ_f(n) -> throw exit succ(n)
;
; function add(Nat, Nat): Nat
;   add(zero(), n)  -> return n
;   add(succ(m), n) -> return succ_f(add(m, n))
;
; "main method" call: `callcc cntname=exit body=add(succ(succ(zero())), succ(succ(succ(succ(zero())))))`
;
; evaluates to succ(succ(succ(zero()))) by leaving the body of callcc after the first invocation of succ_f
;
;
; alternatively, the "main method" call: `callcc cntname=exit body=add(succ(succ(zero())), succ(succ(succ(succ(zero())))))`
;
; evaluates to succ(succ(succ(succ(zero())))) since succ_f is never entered, so there are no "side effects"

(primitive-print
 #s(nat)
 (primitive-apply-callcc
  (primitive-fun
   #s(pair-nat-nat) #s(nat)
   (((p-pairnatnat #s(pair-nat-nat) ((p-zero #s(nat) () () ()) (p-var #s(nat) () () ())) () ())
     (primitive-return #s(nat) (primitive-const (var #s(nat) 0 () ()))))
    ((p-pairnatnat #s(pair-nat-nat) ((p-succ #s(nat) ((p-var #s(nat) () () ())) () ()) (p-var #s(nat) () () ())) () ())
     (primitive-return
      #s(nat)
      (primitive-apply
       ; corresponds to succ_f
       (primitive-fun
        #s(nat) #s(nat)
        (((p-var #s(nat) () () ()) (primitive-throw 0 #s(nat) (primitive-const (succ #s(nat) ((var #s(nat) 0 () ())) () ()))))))
       (primitive-reccall #s(pair-nat-nat) #s(nat) 0
                          (pairnatnat #s(pair-nat-nat) ((var #s(nat) 0 () ()) (var #s(nat) 1 () ())) () ()))
       )))))
  (primitive-const (pairnatnat #s(pair-nat-nat)
                               ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())
                                ;(zero #s(nat) () () ())
                                (succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())) () ())) () ()))
                               () ()))))
