; Nathan Egan
; 872762525

#lang racket

; tools
(require parser-tools/lex
         parser-tools/yacc)

; tokens
(define-tokens v (ID
                  VALUE
                  UNARYOP
                  BINARYOP
                  LAMBDA
                  LET
                  IN
                  IF
                  THEN
                  ELSE
                  CALL
                  WITH))

; empty tokens
(define-empty-tokens e (LEFTPAREN
                        RIGHTPAREN
                        EOF))

; lexer
(define constantBoolLexer
  (lexer
    ["true" (token-VALUE 'true)]
    ["false" (token-VALUE 'false)]
    [#\( (token-LEFTPAREN)]
    [#\) (token-RIGHTPAREN)]
    ["and" (token-BINARYOP 'and)]
    ["or" (token-BINARYOP 'or)]
    ["xor" (token-BINARYOP 'xor)]
    ["not" (token-UNARYOP 'not)]
    ["lambda" (token-LAMBDA 'lambda)]
    ["let" (token-LET 'let)]
    ["in" (token-IN 'in)]
    ["if" (token-IF 'if)]
    ["then" (token-THEN 'then)]
    ["else" (token-ELSE 'else)]
    ["call" (token-CALL 'call)]
    ["with" (token-WITH 'with)]
    [whitespace (constantBoolLexer input-port)]
    [(concatenation alphabetic (repetition 0 +inf.0 (union alphabetic numeric))) (token-ID lexeme)]
    [(eof) (token-EOF)]
    ))

; structs
(struct andOpExpr (argOne argTwo) #:transparent)
(struct orOpExpr (argOne argTwo) #:transparent)
(struct xorOpExpr (argOne argTwo) #:transparent)
(struct unaryOpExpr (argOne) #:transparent)
(struct lambdaExpr (argOne argTwo) #:transparent)
(struct conditionExpr (argOne argTwo argThree) #:transparent)
(struct letOneExpr (argOne argTwo argThree) #:transparent)
(struct letTwoExpr (argOne argTwo argThree) #:transparent)
(struct callExpr (argOne argTwo) #:transparent)
(struct idExpr (value) #:transparent)
(struct valueExpr (value) #:transparent)

; parser
(define expparser
  (parser
    (start exp)
    (end EOF)
    (tokens v e)
    (error void)
    (grammar
      (exp
        ((LEFTPAREN exp BINARYOP exp RIGHTPAREN)
         (cond
           [(equal? $3 'and) (andOpExpr $2 $4)]
           [(equal? $3 'or) (orOpExpr $2 $4)]
           [else (xorOpExpr $2 $4)]))
        ((LEFTPAREN UNARYOP exp RIGHTPAREN) (unaryOpExpr $3))
        ((LEFTPAREN LET LEFTPAREN id exp RIGHTPAREN IN exp RIGHTPAREN)(letOneExpr $4 $5 $8))
        ((LEFTPAREN LET LEFTPAREN id lambda RIGHTPAREN IN exp RIGHTPAREN)(letTwoExpr $4 $5 $8))
        ((LEFTPAREN IF exp THEN exp ELSE exp RIGHTPAREN)(conditionExpr $3 $5 $7))
        ((LEFTPAREN CALL id WITH exp RIGHTPAREN)(callExpr $3 $5))
        ((ID)(idExpr $1))
        ((VALUE) (valueExpr $1))
       )
      (lambda
        ((LEFTPAREN LAMBDA LEFTPAREN ID RIGHTPAREN exp RIGHTPAREN) (lambdaExpr $4 $6))
       )
      (id
       ((ID) (idExpr $1))
       )
    ))
 )

; feed lexer input
(define (lex-this lexer input)
  (lambda () (lexer input)))

; evaluation tests
; use (eval andTest) to execute test
(define andTest (open-input-string "(true and true)"))
(define orTest (open-input-string "(true or false)"))
(define xorTest (open-input-string "(true xor false)"))
(define binaryTest (open-input-string "((true or false) and true)"))
(define notTest (open-input-string "(not false)"))
(define valueTest (open-input-string "true"))
(define letTest (open-input-string "(let (foo true) in (foo and true))"))
(define ifTest (open-input-string "(if false then true else false)"))
(define letLambdaTest (open-input-string "(let (foo (lambda (test) (test and true))) in (call foo with false))"))

; test evaluater with file input (file must be in same directory with name of "file.txt")
; (eval finput)
(define finput (open-input-file "file.txt"))

; helper to make evalutation calls easy
(define (eval input)
  (evaluate (expparser (lex-this constantBoolLexer input)) (empty-env)))

; evaluator
(define (evaluate aTree env)
  (match aTree
    [(valueExpr a) (equal? a 'true)]
    [(unaryOpExpr a) (not (evaluate a env))]
    [(andOpExpr a b) (and (evaluate a env) (evaluate b env))]
    [(orOpExpr a b) (or (evaluate a env) (evaluate b env))]
    [(xorOpExpr a b) (xor (evaluate a env) (evaluate b env))]
    [(conditionExpr a b c) (cond
                             [(and (and (evaluate a env) #t) (evaluate b env))]
                             [else (evaluate c env)])]
    [(letOneExpr a b c) (evaluate c (extend-env (idExpr-value a) (evaluate b env) env))]
    [(idExpr a) (apply-env env a)]
    [(letTwoExpr a b c) (evaluate (second (evaluate b env)) (extend-env (first (evaluate b env)) (first (evaluate c env)) env))]
    [(callExpr a b) (cons (evaluate b env) '())]
    [(lambdaExpr a b) (cons a (cons b '()))]
   ))

; Implementation of Environment
; empty-env : () -> Env
(define empty-env
  (lambda () (list 'empty-env)))

; extend-env : Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((equal? (car env) 'empty-env)
        (error "No Binding Found:" search-var))
      ((equal? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (equal? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (error "Invalid Environment: ~s"  env)))))