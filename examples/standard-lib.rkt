#lang s-exp "../cu/cu.rkt"

(data
 #s(recursive)

 (nat
  ([zero () () ()]
   [succ (nat) () ()]))

 (pair-nat
  ([pair (nat nat) () ()]))
 )

(codata
 #s(recursive)

 (shift-nat
  ([shift () () (nat)]))

 (shift-pair-nat
  ([shiftp () () (pair-nat)]))
 
 (nat-fun
  ([apply (shift-nat) (shift-pair-nat) ()]))
 )
