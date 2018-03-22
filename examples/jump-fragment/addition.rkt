#lang s-exp "type-defs.rkt"

(require "../../cu/profiles/jump-sugar.rkt")

; The code below corresponds 1:1 to the following program in the data fragment enhanced
; with arbitrary stack jumps.
;
; function succ_f(Nat): Nat
;   succ_f(n) -> return@succ_f succ(n)
;
; function add(Nat, Nat): Nat
;   add(zero(), n)  -> return@add n
;   add(succ(m), n) -> return@"main" succ_f(add(m, n))
;
; "main method" call: `add(succ(succ(zero())), succ(succ(succ(succ(zero())))))`
;
; produces succ(succ(succ(zero()))), by the following reduction:
;
;     add(succ(succ(zero())), succ(succ(succ(succ(zero())))))
; stack: "main"
;
; ->  succ_f(add(succ(succ(zero()))), succ(succ(succ(succ(zero()))))))
; stack: add{-}, "main"
;
; ->  succ_f(add(succ(zero()), succ(succ(succ(succ(zero()))))))
; stack: add{succ_f[]}, add{[]}, "main"
;
; ->  succ_f(add(zero(), succ(succ(succ(succ(zero()))))))
; stack: add{succ_f[]}, add{succ_f[]}, add{[]}, "main"
;
; ->  succ(succ(succ(succ(zero()))))
; stack: add{succ_f[]}, add{succ_f[]}, add{succ_f[]}, add{[]}, "main"
; return@add
;
; ->  succ(succ(succ(succ(zero()))))
; stack: add{succ_f[]}, add{succ_f[]}, add{succ_f[]}, add{[]}, "main"
; return@"main"

(primitive-print
 #s(nat)
 (primitive-apply
  (primitive-fun
   #s(pair-nat-nat) #s(nat)
   (((p-pairnatnat #s(pair-nat-nat) ((p-zero #s(nat) () () ()) (p-var #s(nat) () () ())) () ())
     (primitive-return 0 #s(nat) (primitive-const (var #s(nat) 0 () ()))))
    ((p-pairnatnat #s(pair-nat-nat) ((p-succ #s(nat) ((p-var #s(nat) () () ())) () ()) (p-var #s(nat) () () ())) () ())
     (primitive-return
      1 #s(nat)
      (primitive-apply
       ; corresponds to succ_f
       (primitive-fun
        #s(nat) #s(nat)
        (((p-var #s(nat) () () ()) (primitive-return 0 #s(nat) (primitive-const (succ #s(nat) ((var #s(nat) 0 () ())) () ()))))))
       (primitive-reccall #s(pair-nat-nat) #s(nat) 0
                          (pairnatnat #s(pair-nat-nat) ((var #s(nat) 0 () ()) (var #s(nat) 1 () ())) () ()))
       )))))
  (primitive-const (pairnatnat #s(pair-nat-nat)
                               ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())
                                (succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((succ #s(nat) ((zero #s(nat) () () ())) () ())) () ())) () ())) () ()))
                               () ()))))
