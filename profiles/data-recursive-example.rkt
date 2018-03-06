#lang s-exp "data.rkt"

(data
 #s(recursive)

 (unit
   ([tt ()]))
 
 (nat
  ([zero ()]
   [succ (nat)]))

 (pair-nat
  ([pair (nat nat)]))

 )
