#lang racket

(require rebellion/streaming/transducer)
(require rebellion/streaming/reducer)
(require racket/list)

(define into-list
  (make-effectful-fold-reducer (λ (lst v)
                                 (cons v lst))
                               list
                               reverse))

(length (transduce (file->lines  "resources/01.txt")
           (mapping string->number)
           (filtering (λ (x) (not (false? x))))
           (windowing 2)
           (filtering (λ (p) (> (second p) (first p))))
           #:into into-list))
