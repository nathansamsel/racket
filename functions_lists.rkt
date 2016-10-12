#lang racket

(require test-engine/racket-tests)

; demote function
; wraps parentheses around each top-level element of a list and return the new list
(define (demote lot)
  (cond [(empty? lot) '()]
        [(= (length lot) 1) (list lot)]
        [else (cons (list (first lot)) (demote (rest lot)))]))

; promote function
; removes one pair of parentheses from around each top-level element of a list
; if element is not a list, it is included in the result, as is
(define (promote lot)
  (cond [(empty? lot) '()]
        [(list? (first lot)) (append (first lot) (promote (rest lot)))]
        [else (cons (first lot) (promote (rest lot)))]))
        

; flatten function
; returns a list of the symbols in the order in which they occur
; Essentially, removes all of the inner parenthesis
(define (flatten lot)
  (cond [(empty? lot) '()]
        [(not (list? lot)) (list lot)]
        [else (append (flatten (first lot)) (flatten (rest lot)))]
        ))

; tests for demote function
(check-expect (demote '()) '())
(check-expect (demote '(1 2 3)) '((1)(2)(3)))
(check-expect (demote '((a)(good)(day))) '(((a))((good))((day))))
(check-expect (demote '(a (more (complicated)) example)) '((a) ((more (complicated)))(example)))

; tests for promote function
(check-expect (promote '((1 2)(3 4))) '(1 2 3 4))
(check-expect (promote '((x (y)) z)) '(x (y) z))
(check-expect (promote '(a b c)) '(a b c))
(check-expect (promote '()) '())

; tests for flatten function
(check-expect (flatten '(a b c)) '(a b c))
(check-expect (flatten '((a) ()(b())()(c))) '(a b c))
(check-expect (flatten '((a b) c (((d)) e))) '(a b c d e))
(check-expect (flatten '()) '())

; run tests
(test)
