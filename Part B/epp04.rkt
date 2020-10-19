; Author  : Pieter van Wyk
; Created : 2020-10-03
; Updated : 2020-10-20
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

; helper function for testing streams (returns the first n elements of a stream)
(define (stream-to-list stream n)
  (cond [(= n 0) null]
        [#t (cons (car (stream)) (stream-to-list (cdr (stream)) (- n 1)))]))

; helper functions for testing facts
(define nats
  (letrec ([aux (λ(x) (λ()(cons x (aux (+ x 1)))))])
    (aux 0)))

(define (fact n)
  (cond [(zero? n) 1]
        [#t (* n (fact (- n 1)))]))

; main
(define facts
  (letrec ([aux (λ(x y) (λ()(cons x (aux (* x y) (+ y 1)))))])
      (aux 1 1)))


; --- problem 2 (b) ---
; Define a stream fibonacci, the first element of which is 0, the second one is 1, and each
; successive element is the sum of two immediately preceding elements.

; helper functions for testing fibonacchi
(define (fibo n)
  (cond [(< n 2) n]
        [#t  (+ (fibo (- n 1)) (fibo (- n 2)))]))

; main
(define fibonacchi
  (letrec ([aux (λ(x y) (λ()(cons x (aux y (+ x y)))))])
      (aux 0 1)))


; --- problem 3 ---
; Write a function stream-until that takes a function f and a stream s, and applies f to the
; values of s in succession until f evaluates to #f. The values after applying f are returned
; as a list.

; helper for testing (takes integer input function f and integer n-max
; and returns a function that evaluates to #f if the input is above
; n-max, otherwise applying f)
(define (f-cond f n-max)
  (λ(n) (cond [(>= n n-max) #f]
              [#t (f n)])))

; main
(define (stream-until f s)
  (cond [(false? (f (car (s)))) null]
        [#t (cons (f (car (s))) (stream-until f (cdr (s))))]))


; --- problem 4 ---
; Write a function stream-map that takes a function f and a stream s, and returns a new stream
; whose values are the result of applying f to the values produced by s.

(define (stream-map f s) (λ() (cons (f (car(s))) (stream-map f (cdr (s))))))


; --- problem 5 ---
; Write a function stream-zip that takes in two streams s1 and s2 and returns a stream that produces
; the pairs that result from the other two streams (so the first value for the result stream will be
; the pair of the first value of s1 and the first value of s2).

; helper : zip function
(define (zip xs ys)
  (cond [(or (null? xs) (null? ys)) null]
        [#t (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))]))

; main
(define (stream-zip s1 s2) (λ() (cons (list (car(s1)) (car(s2))) (stream-zip (cdr(s1)) (cdr(s2))))))
              
; END
