#lang s-exp "../cu.rkt"

(data
 #s(recursive)

 (nat
  ([zero () () ()]
   [succ (nat) () ()]))

 (pair-nat-nat
  ([pairnatnat (nat nat) () ()]))

 (nat-to-nat
  ([nattonat (nat) (nat) ()]))
 )

(codata
 #s(recursive)

 (shift-nat
  ([shiftnat () () (nat)]))

 (shift-pair-nat-nat
  ([shiftpairnatnat () () (pair-nat-nat)]))
 )
