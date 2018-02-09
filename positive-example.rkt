#lang s-exp "positive.rkt"

(data
 
 (bool
  ([true () ()]
   [false () ()]))

 (maybe-bool
  ([some (bool) ()]
   [none () ()]))

 (prod-bool
  ([pair (bool bool) ()]))

 (subtraction-bool
  ([unapply (bool) (bool)]))
 
 )
