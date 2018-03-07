#lang s-exp "data-example.rkt"

(lambda #s(bool) (((p-var #s(bool) () ()) (cmd (lambda #s(abstr-bool-unit-bool)
                                                 (((p-abstr-bool-unit-bool #s(abstr-bool-unit-bool)
                                                                            ((p-var #s(bool) () ()) (p-tt #s(unit) () ()))
                                                                          ((p-var #s(bool) () ())))
                                                    (cmd (nvar #s(bool) 0 ()) (var #s(bool) 0 ())))
                                                   (cmd daemon #s(impossible) ())))
                                               (abstr-bool-unit-bool #s(abstr-bool-unit-bool)
                                                                      ((var #s(bool) 0 ()) (tt #s(unit) () ()))
                                                                      ((lambda #s(bool) (((p-var #s(bool) () ()) (cmd daemon #s(print) ((var #s(bool) 1 ()))))
                                                                                         (cmd daemon #s(impossible) ())))))))
                  (cmd daemon #s(print) ((var #s(bool) 0 ())))))
