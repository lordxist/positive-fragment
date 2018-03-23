#lang s-exp "type-defs.rkt"

(require "../../cu/profiles/jump-sugar.rkt")

(primitive-print
 #s(bool)
 (primitive-apply
  (primitive-fun
   #s(word) #s(bool)
   (((p-var #s(word) () () ())
     (primitive-return
      #s(bool)
      (primitive-apply-callcc
       (primitive-fun
        #s(maybe-word) #s(bool)
        (((p-none #s(maybe-word) () () ()) (primitive-return #s(bool) (primitive-const (true #s(bool) () () ()))))
         ((p-some #s(maybe-word) ((p-var #s(word) () () ())) () ()) (primitive-return #s(bool) (primitive-const (false #s(bool) () () ()))))))
       (primitive-apply
        (primitive-fun
         #s(word) #s(maybe-word)
         (((p-empty #s(word) () () ())
           (primitive-return #s(maybe-word) (primitive-const (none #s(maybe-word) () () ()))))
          ((p-cons #s(word) ((p-l #s(parens) () () ()) (p-var #s(word) () () ())) () ())
           (primitive-apply
            (primitive-fun
             #s(maybe-word) #s(maybe-word)
             (((p-none #s(maybe-word) () () ()) (primitive-throw 0 #s(bool) (primitive-const (false #s(bool) () () ()))))))
            (primitive-reccall #s(word) #s(maybe-word) 0 (var #s(word) 0 () ()))))
          ((p-cons #s(word) ((p-r #s(parens) () () ()) (p-var #s(word) () () ())) () ())
           (primitive-return #s(maybe-word) (primitive-const (some #s(maybe-word) ((p-var #s(word) () () ())) () ()))))))
        (primitive-const (var #s(word) 0 () ()))))))))
  (primitive-const (cons #s(word) ((l #s(parens) () () ()) (cons #s(word) ((r #s(parens) () () ()) (empty #s(word) () () ())) () ())) () ()))))
