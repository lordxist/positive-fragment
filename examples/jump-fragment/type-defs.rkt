#lang s-exp "../../cu/cu.rkt"

(data
 #s(recursive)

 (nat
  ([zero () () ()]
   [succ (nat) () ()]))

 (pair-nat-nat
  ([pairnatnat (nat nat) () ()]))

 (nat-to-nat
  ([nattonat (nat) (nat) ()]))

 (pair-nat-nat-to-nat
  ([pairnatnattonat (pair-nat-nat) (nat) ()]))

 (bool
  ([true () () ()]
   [false () () ()]))

 (parens
  ([l () () ()]
   [r () () ()]))

 (word
  ([empty () () ()]
   [cons (parens word) () ()]))

 (maybe-word
  ([none () () ()]
   [some (word) () ()]))

 (word-to-bool
  ([wordtobool (word) (bool) ()]))

 (maybe-word-to-bool
  ([maybewordtobool (maybe-word) (bool) ()]))

 (word-to-maybe-word
  ([wordtomaybeword (word) (maybe-word) ()]))

 (maybe-word-to-maybe-word
  ([maybewordtomaybeword (maybe-word) (maybe-word) ()]))
 )

(codata
 #s(recursive)

 (shift-nat
  ([shiftnat () () (nat)]))

 (shift-pair-nat-nat
  ([shiftpairnatnat () () (pair-nat-nat)]))

 (shift-word
  ([shiftword () () (word)]))

 (shift-maybe-word
  ([shiftmaybeword () () (maybe-word)]))

 (shift-bool
  ([shiftbool () () (bool)]))

 (shift-parens
  ([shiftparens () () (parens)]))
 )
