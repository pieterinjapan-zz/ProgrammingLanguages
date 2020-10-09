#lang racket
(provide (all-defined-out))


; --- problem 1 ---
; Write a function palindromic that takes a list of numbers and evaluates to a list of
; numbers of the same length, where each element is obtained as follows: the first element
; should be the sum of the first and the last elements of the original list, the second one
; should be the sum of the second and second to last elements of the original list, etc.
; Example: (palindromic (list 1 2 4 8)) evaluates to (list 9 6 6 9).

; helper (add two lists element wise)
(define (add-lists xs ys)
  (if (or (null? xs) (null? ys))
      null
      (cons (+ (car xs) (car ys))(add-lists (cdr xs) (cdr ys)))))

; main
(define (palindromic xs)
  (add-lists xs (reverse xs)))
