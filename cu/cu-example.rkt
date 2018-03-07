#lang s-exp "cu.rkt"

(data
 #s(recursive)

 (nat
  ([zero () ()]
   [succ (nat) ()]))
 )

(codata
 #s(recursive)

 (bot ())
 
 (pair-fun
  ([fun (bot) (bot)]))
 )
