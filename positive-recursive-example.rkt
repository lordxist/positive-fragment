#lang s-exp "positive.rkt"

(data #s(recursive)

 (nat
  ([zero () ()]
   [succ (nat) ()]))

 (nat-fun
  ([fun (nat nat) (nat)]))

 )
