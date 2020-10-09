#lang racket
(provide (all-defined-out))
(require rackunit)
(require "epp04.rkt")

(define tests
  (test-suite
   "unit tests for extra practice problems"

   ; unit tests for problem 1
   (check-equal? (add-lists '(1 2 3) '(1 2 3)) '(2 4 6) "add-list test 1")
      
   ))

(require rackunit/text-ui)

;; runs the test
(run-tests tests)
