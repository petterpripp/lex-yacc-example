#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER))
(define-empty-tokens op-tokens (EOF ADD SUBTRACT PRODUCT DIVISION POWER NEG OP CP NEWLINE))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ (:& (:~ #\newline) whitespace)) (return-without-pos (next-token input-port))]   
   [#\+ (token-ADD)]
   [#\- (token-SUBTRACT)]
   [#\* (token-PRODUCT)]
   [#\/ (token-DIVISION)]      
   [#\^ (token-POWER)]
   ;[#\n (token-NEG)]
   [#\( (token-OP)]
   [#\) (token-CP)]
   [#\newline (token-NEWLINE)]
   [(:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]))

(provide value-tokens op-tokens next-token)   
