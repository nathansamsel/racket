#lang racket

(require parser-tools/lex
         parser-tools/yacc)

(define-tokens v (ID
                  VALUE
                  UNARYOP
                  BINARYOP
                  ))

(define-empty-tokens e (EOF
                        LEFTPAREN
                        RIGHTPAREN
                        LAMBDA
                        LET
                        IN
                        IF
                        THEN
                        ELSE
                        CALL
                        WITH))

(define constantBoolLexer
  (lexer
    ["true" (token-VALUE 'true)]
    ["false" (token-VALUE 'false)]
    [#\( (token-LEFTPAREN)]
    ["and" (token-BINARYOP 'and)]
    ["or" (token-BINARYOP 'or)]
    ["xor" (token-BINARYOP 'xor)]
    ["not" (token-UNARYOP 'not)]
    ["lambda" (token-BINARYOP 'lambda)]
    ["let" (token-BINARYOP 'let)]
    ["in" (token-BINARYOP 'in)]
    ["if" (token-BINARYOP 'if)]
    ["then" (token-BINARYOP 'then)]
    ["else" (token-BINARYOP 'else)]
    ["call" (token-BINARYOP 'call)]
    ["with" (token-BINARYOP 'in)]
    [whitespace (constantBoolLexer input-port)]
    [(concatenation alphabetic (repetition 1 +inf.0 (union alphabetic numeric))) (token-ID lexeme)]
    ))

(define (getTokens lex in)
  (let [(token (lex in))]
    (cond [(equal? token 'eof) '()]
          [else (cons token (getTokens lex in))])))