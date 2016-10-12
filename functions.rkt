#lang racket

(require test-engine/racket-tests)

; predicate function ascending? takes a list
; return #t if strictly ascending, #f otherwise
(define (ascending? lon)
  ; if null, list lon is in order by definition
  (cond ((null? lon) #t)
        ; if length of list lon is 1, list is in order by definition
        ((eqv? (length lon) 1) #t)
        ; if first element is larger than second, not in ascending order, return #f
        ((> (first lon) (second lon)) #f)
        ; run another step of asceding?
        ; recursion works because of how lists are structured
        ; lists are combinations of pairs that create a list (linked list)
        ; first element is a list element, second element is a list
        ; this allows us to use first, second, rest to compare elements and step through the list recursively
        (else (ascending? (rest lon)))))

; function revers-pairs takes a list of pairs
; reveses pairs in list
(define (reverse-pairs lon)
  ;base case list is empty
  (cond [(empty? lon) lon]
        ;create list out of second element, first element of first pair and recursively call reverse-pairs with rest
        [else (cons (list (second (first lon)) (first (first lon))) (reverse-pairs (rest lon)))]))

; function translate-digits takes a list of digits 0 - 9
; translates each digit to corresponding char
(define (translate-digits lon)
  ;base case the list is empty
  (cond [(empty? lon) lon]
        ;test for each number, add its translation to a list and recursively call translate-digits
        [(equal? (first lon) 1) (cons 'One (translate-digits (rest lon)))]
        [(equal? (first lon) 2) (cons 'Two (translate-digits (rest lon)))]
        [(equal? (first lon) 3) (cons 'Three (translate-digits (rest lon)))]
        [(equal? (first lon) 4) (cons 'Four (translate-digits (rest lon)))]
        [(equal? (first lon) 5) (cons 'Five (translate-digits (rest lon)))]
        [(equal? (first lon) 6) (cons 'Six (translate-digits (rest lon)))]
        [(equal? (first lon) 7) (cons 'Seven (translate-digits (rest lon)))]
        [(equal? (first lon) 8) (cons 'Eight (translate-digits (rest lon)))]
        [(equal? (first lon) 9) (cons 'Nine (translate-digits (rest lon)))]
        [(equal? (first lon) 0) (cons 'Zero (translate-digits (rest lon)))]))

; function duplicate takes a number n and expersion exp
; duplicates exp n times
(define (duplicate n exp)
  ;base case duplicate 0 times
  (cond [(equal? n 0) '()]
        ;add experession to list and recursively call duplicate until n gets to 0
        [else (cons exp (duplicate (- n 1) exp))]))
  
; function count-symbol takes a symbol s and a list los
; returns the count of how many times symbol occurs
(define (count-symbol s los)
  ;base case los is empty
 (cond [(empty? los) 0]
       ;if symbol is found, add one and recursively call count-symbols
       [(equal? (first los) s) (+ 1 (count-symbol s (rest los)))]
       ;if there is a list embedded in los (like search example in class)
       ;then count symbols in that list and add 1 and recursivly call
       [(and (list? (first los)) (count-symbol s (first los))) (+ 1 (count-symbol s (rest los)))]
       ;if not found symbol or embedded list, recursively call on rest
       [else (count-symbol s (rest los))]))

; 3 tests for ascending? predicate function
(check-expect (ascending? '(1 3 5 7)) #t)
(check-expect (ascending? '(7 5 3 1)) #f)
(check-expect (ascending? '(10 11 12 14)) #t)

; 3 tests for reverse-pairs function
(check-expect (reverse-pairs '((a b)(1 3)(e f))) '((b a)(3 1)(f e)))
(check-expect (reverse-pairs '((x y)(d z)(5 6))) '((y x)(z d)(6 5)))
(check-expect (reverse-pairs '((test what)(8 7)(d c))) '((what test)(7 8)(c d)))

; 3 tests for translate-digits function
(check-expect (translate-digits '(1 3 5 7 0)) '(One Three Five Seven Zero))
(check-expect (translate-digits '(2 6 4 3 2 1)) '(Two Six Four Three Two One))
(check-expect (translate-digits '(1 2 3)) '(One Two Three))

; 3 tests for duplicate function
(check-expect (duplicate 3 '(A (B) C)) '((A (B) C) (A (B) C) (A (B) C)))
(check-expect (duplicate 2 'Hello) '(Hello Hello))
(check-expect (duplicate 4 '(1 2 3)) '((1 2 3) (1 2 3) (1 2 3) (1 2 3)))

; 3 tests for count-symbol function
(check-expect (count-symbol 'A '(B A A C A)) 3)
(check-expect (count-symbol 'A '(B A (A B)((A))A)) 4)
(check-expect (count-symbol 'TEST '(TEST TEST TEST TEST TEST NOTTEST)) 5)

;run tests
(test)