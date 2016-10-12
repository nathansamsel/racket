#lang racket
(require test-engine/racket-tests)

;;; Factorial Function
;;; Stub
(define (fact n)
  (if(= n 0)
    1
    (* n (fact (- n 1)))))

(define theList '(1 2 3 4 5 6))

(define (search n listOfNum)
  (cond [(empty? listOfNum) false]
        [(equal? n (first listOfNum)) true]
        [(and (list? (first listOfNum))
              (search n (first listOfNum)))]
        [else (search n (rest listOfNum))]))
  

;tests
(check-expect (fact 5) 120)
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)

;run the tests
(test)
