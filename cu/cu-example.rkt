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
  ([fun (bot) (bot) ()]))

 (shift-nat
  ([shift () () (nat)]))
 
 (nat-fun
  ([apply (shift-nat) (shift-nat) ()]))
 )
