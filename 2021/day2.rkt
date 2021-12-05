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

(define (fold-commands accum command)
  (match command
    ;; down X increases your aim by X units.
    [(list (regexp #rx"down") x) (map + accum (list 0 0 x))]
    ;; up X decreases your aim by X units.
    [(list (regexp #rx"up") x)   (map + accum (list 0 0 (* -1 x)))]

    ;; forward X does two things:
    ;;    It increases your horizontal position by X units.
    ;;    It increases your depth by your aim multiplied by X.
    [(list (regexp #rx"forward") x)
     (let ([aim (third accum)])
       (list (+ x (first accum))
             (+ (* aim x) (second accum))
             aim))]
    [_ accum]))

(define summed-vec
  (transduce (file->lines  "resources/02.txt")
             (mapping (λ (x) (string-split x " ")))
             (mapping (λ (x) (list (first x)
                                   (string->number (second x)))))
             (folding fold-commands (list 0 0 0))
             #:into into-last))


(define output (* (first (present-value summed-vec))
                  (second (present-value summed-vec))))
