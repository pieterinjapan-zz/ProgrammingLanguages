; Author  : Pieter van Wyk
; Created : 2020-10-10
; Updated : 2020-10-15
;
; Solutions to the extra practice problems of week 1 of part B 
;

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
  (cond [(or (null? xs) (null? ys)) null]
        [#t (cons (+ (car xs) (car ys)) (add-lists (cdr xs) (cdr ys)))]))

; main
(define (palindromic xs)
  (add-lists xs (reverse xs)))


; --- problem 2 (a) ---
; Define a stream facts, the stream of factorials of the natural
; numbers starting at 0.

(define facts
  (letrec ([aux (λ(x y) (λ()(cons x (aux (* x y) (+ y 1)))))])
      (aux 1 1)))

; helper functions for testing facts
(define nats
  (letrec ([aux (λ(x) (λ()(cons x (aux (+ x 1)))))])
    (aux 0)))

(define (fact n)
  (cond [(zero? n) 1]
        [#t (* n (fact (- n 1)))]))

; END
