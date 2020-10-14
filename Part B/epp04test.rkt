; Author  : Pieter van Wyk
; Created : 2020-10-03
; Updated : 2020-10-15
;
; Unit tests for the extra practice problems of week 1 of part B 
;

#lang racket
(provide (all-defined-out))
(require rackunit)
(require rackunit/text-ui)
(require "epp04.rkt")

; helper function for testing streams (returns the first n elements of a stream)
(define (stream-to-list stream n)
  (cond [(= n 0) null]
        [#t (cons (car (stream)) (stream-to-list (cdr (stream)) (- n 1)))]))

(define tests
  (test-suite
   "unit tests for extra practice problems"

   ; unit tests for problem 1
   (check-equal? (add-lists '(1 2 3) '(1 2 3)) '(2 4 6) "add-list test 1")
   (check-equal? (add-lists '(1 2 3) '(3 2 1)) '(4 4 4) "add-list test 2")
   (check-equal? (add-lists '(1 2) '(1 2 3)) '(2 4)     "add-list test 3")
   (check-equal? (add-lists '(1 2 3) '(2 3)) '(3 5)     "add-list test 4")

   (check-equal? (palindromic '(1 2 4 8)) '(9 6 6 9)     "palindromic test 1")
   (check-equal? (palindromic '(7 4 2 1)) '(8 6 6 8)     "palindromic test 2")
   (check-equal? (palindromic '(6 9 1 4)) '(10 10 10 10) "palindromic test 3")
   (check-equal? (palindromic '(5 2 7 3)) '(8 9 9 8)     "palindromic test 4")

   ; unit tests for problem 2 (a)
   (check-equal? (stream-to-list nats 5)  '(0 1 2 3 4)           "nats test 1")
   (check-equal? (stream-to-list nats 7)  '(0 1 2 3 4 5 6)       "nats test 2")
   (check-equal? (stream-to-list nats 9)  '(0 1 2 3 4 5 6 7 8)   "nats test 3")
   (check-equal? (stream-to-list nats 10) '(0 1 2 3 4 5 6 7 8 9) "nats test 4")

   (check-equal? (stream-to-list facts 5)  '(1 1 2 6 24)                       "facts test 1")
   (check-equal? (stream-to-list facts 10) (map fact '(0 1 2 3 4 5 6 7 8 9))   "facts test 2")
   (check-equal? (stream-to-list facts 15) (map fact (stream-to-list nats 15)) "facts test 3")
   (check-equal? (stream-to-list facts 20) (map fact (stream-to-list nats 20)) "facts test 4")

   ; unit tests for problem 2 (b)
   (check-equal? (stream-to-list fibonacchi 5)  '(0 1 1 2 3)                        "facts test 1")
   (check-equal? (stream-to-list fibonacchi 10) (map fibo '(0 1 2 3 4 5 6 7 8 9))   "facts test 2")
   (check-equal? (stream-to-list fibonacchi 15) (map fibo (stream-to-list nats 15)) "facts test 3")
   (check-equal? (stream-to-list fibonacchi 20) (map fibo (stream-to-list nats 20)) "facts test 4")
      
   ))

;; runs the test
(run-tests tests)

; END
