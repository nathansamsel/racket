#lang racket

; run with
; (getTokens constantBoolLexer input)
; to get a list back

; or run with
; test
; test1
; test2
; to get a list back of test input thats defined at the bottom of the file

(require parser-tools/lex
         parser-tools/yacc
         parser-tools/lex-sre)

(define-tokens v (ID
                  VALUE
                  UNARYOP
                  BINARYOP
                  ))

(define-empty-tokens e (EOF
                        LEFTPAREN
                        RIGHTPAREN))

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
    [whitespace (constantBoolLexer input-port)]
    [(concatenation alphabetic (repetition 1 +inf.0 (union alphabetic numeric))) (token-ID lexeme)]
    ))

(define (getTokens lex in)
  (let [(token (lex in))]
    (cond [(equal? token 'eof) '()]
          [else (cons token (getTokens lex in))])))


(define input (open-input-string "Wwhatverdfs231adf ( and false"))
(define input1 (open-input-string "( test and flase )"))
(define input2 (open-input-string "true xor not false"))

(define test (getTokens constantBoolLexer input))
(define test1 (getTokens constantBoolLexer input1))
(define test2 (getTokens constantBoolLexer input2))