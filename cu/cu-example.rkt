#lang s-exp "cu.rkt"

(data
 #s(recursive)

 (nat
  ([zero () () ()]
   [succ (nat) () ()]))
 )

(codata
 #s(recursive)

 (bot ())
 
 (bot-fun
  ([bot-fun (bot) (bot) ()]))

 (shift-nat
  ([shift-nat () () (nat)]))
 
 (nat-fun
  ([apply (shift-nat) (shift-nat) ()]))
 )
