#lang racket

(require rebellion/streaming/transducer)
(require rebellion/streaming/reducer)
(require racket/list)

(define into-list
  (make-fold-reducer (λ (lst v) (cons v lst))
                     (list)))

(length (transduce (file->lines  "resources/01.txt")
            (mapping string->number)
            (filtering (λ (x) (not (false? x))))
            (windowing 3)
            (windowing 2)
            (mapping (λ (x)
                       (list
                        (apply + (first x))
                        (apply + (second x)))))
            (filtering (λ (x) (> (second x) (first x))))
            #:into into-list))

