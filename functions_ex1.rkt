#lang racket
(define (merge a b)
  (cond [(empty? a) b]
        [(empty? b) a]
        [(< (first a) (first b))
         (cons (first a) (merge (rest a) b))]
        [else (cons (first b) (merge a (rest b)))]))

(define (split a)
  (cond [(empty? a) '( () ())]
        [(= (length a) 1) (list a '())]
        [else (let ([foo (split (rest (rest a)))])
                (list (cons (first a)(first foo))
                      (cons (first (rest a)) (first (rest foo)))))]))

(define (mergeSort a)
  (cond [(empty? a) a]
        [(= (length a) 1) a]
        [else (split a)
  )
 
(define (f z)
  (let ([x 5]
        [y 10])
    (+ x y z)))