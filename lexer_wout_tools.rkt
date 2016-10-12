#lang racket
(define (clexer s)
  (cond [(empty? s) s]
        [(equal?  (first s) #\S)
         (cons 'token-S (clexer (rest s)))]
        [(equal?  (first s) #\K) 'token-K
         (cons 'token-K (clexer (rest s)))]
        [(equal?  (first s) #\I) 'token-I
         (cons 'token-I (clexer (rest s)))]
        [(equal?  (first s) #\() 'token-LEFTPAREN
         (cons 'token-LEFTPAREN (clexer (rest s)))]
        [(equal?  (first s) #\)) 'token-RIGHTPAREN
         (cons 'token-RIGHTPAREN (clexer (rest s)))]
        [else (error "Error: Unknown character" (first s))]))

(define (lamlexer s)
  (cond [(empty? s) s]
        [(equal?  (first s) #\λ)
         (cons 'token-LAMBDA (lamlexer (rest s)))]
        [(equal?  (first s) #\.) 'token-
         (cons 'token-. (lamclexer (rest s)))]
        [(equal?  (first s) #\I) 'token-I
         (cons 'token-I (lamclexer (rest s)))]
        [(equal?  (first s) #\() 'token-LEFTPAREN
         (cons 'token-LEFTPAREN (lamlexer (rest s)))]
        [(equal?  (first s) #\)) 'token-RIGHTPAREN
         (cons 'token-RIGHTPAREN (lamlexer (rest s)))]
        [else (error "Error: Unknown character" (first s))]))