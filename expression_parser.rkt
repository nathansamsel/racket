#lang racket

; run with
; (evaluate (expparser (lex-this explexer input)))

(require parser-tools/lex
         parser-tools/yacc)

(define-empty-tokens e (PLUS
                        MULT
                        LEFTPAREN
                        RIGHTPAREN
                        EOF))

(define-tokens v (NUMBER))

(define explexer
  (lexer
    [numeric (token-NUMBER lexeme)]
    [#\+ (token-PLUS)]
    [#\* (token-MULT)]
    [#\( (token-LEFTPAREN)]
    [#\) (token-RIGHTPAREN)]
    [whitespace (explexer input-port)]
    [(eof) (token-EOF)]
    ))

(struct numExpr (value) #:transparent)
(struct plusExpr (argOne argTwo) #:transparent)
(struct multExpr (argOne argTwo) #:transparent)

(define expparser
  (parser
    (start exp)
    (end EOF)
    (tokens v e)
    (error void)
    (grammar
      (exp
        ((exp PLUS mulexp) (plusExpr $1 $3))
        ((mulexp) $1)
       )
      (mulexp
        ((mulexp MULT exp) (multExpr $1 $3))
        ((LEFTPAREN exp RIGHTPAREN) $2)
        ((NUMBER) (numExpr (string->number $1)))
       )
      )
    ))

(define (lex-this lexer input)
  (lambda () (lexer input)))

(define input (open-input-string "9 + 9 + 9"))

(define (evaluate aTree)
  (match aTree
        [(plusExpr a b) (+ (evaluate a)
                           (evaluate b))]
        [(multExpr a b) (* (evaluate a)
                           (evaluate b))]
        [(numExpr a) a]))

;; Implement an environment (functional)
;; run with
;; (apply-env (extend-env 'x 5 (empty-env)) 'x)
;; finds value of x = 5

(define (empty-env)
  (lambda (searchVar)
    (error "No Binding Found" searchVar)))

(define (extend-env savedVar savedVal savedEnv)
  (lambda (searchVar)
    (if (equal? searchVar savedVar) savedVal
        (apply-env savedEnv searchVar))))

(define (apply-env env searchVar)
  (env searchVar))

; empty-env : () -> Environment
; data structure implementation of environment

(define (empty-env-ds)
  (list 'empty-env))

; extend-env : Var x Value x Environment -> Environment

(define (extend-env-ds var val env)
  (list 'extend-env var val env))

; apply-env : Environment x Car -> Value

(define (apply-env-ds env var searchVar)
  (cond [(equal? (first env) 'empty-env)
                 (error "No Binding Found" searchVar)]
        [(equal? (first env) 'extend-env)
                 ; used let to save typing over and over
                 (let ([savedVar (second env)]
                       [savedVal (third env)]
                       [savedEnv (fourth env)])
                   (if (equal? searchVar savedVar)
                       savedVal
                       (apply-env-ds savedEnv searchVar)))]
        [else (error "Invalid Environment" env)]))
