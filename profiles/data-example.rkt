#lang s-exp "data.rkt"

(data
 
 (bool
  ([true ()]
   [false ()]))

 (maybe-bool
  ([some (bool)]
   [none ()]))

 (prod-bool
  ([pair (bool bool)]))
 
 )
