#lang racket

(provide (all-defined-out))

(struct profile (cont-in-cmd))
(struct limitation (domain prefix error-descr))

(struct shifts (opposite-lambda opposite-var opposite-cmd))
