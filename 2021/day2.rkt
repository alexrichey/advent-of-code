#lang racket

(require rebellion/streaming/transducer)
(require rebellion/streaming/reducer)
(require rebellion/base/option)
(require racket/list)
(require racket/match)
(require racket/string)
(require racket/vector)

(define into-list
  (make-effectful-fold-reducer (λ (lst v) (cons v lst)) list reverse))

(define (dir->vector dir)
  (match dir
   [(list (regexp #rx"forward") x) (vector x 0)]
   [(list (regexp #rx"down") x)    (vector 0 x)]
   [(list (regexp #rx"up") x)      (vector 0 (* -1 x))]
   [_                              (vector 0 0)]))

(define summed-vec
  (transduce (file->lines  "resources/02.txt")
             (mapping (λ (x) (string-split x " ")))
             (mapping (λ (x) (list (first x)
                                   (string->number (second x)))))
             (mapping dir->vector)
             (folding (λ (x y) (vector-map + x y))
                      (vector 0 0))
             (mapping (λ (x) (vector-map * x)))
             #:into into-last))

(apply * (vector->list (present-value summed-vec)))
