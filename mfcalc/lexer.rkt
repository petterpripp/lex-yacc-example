#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "funs.rkt")


(define-tokens value-tokens (NUMBER VAR FUN))
(define-empty-tokens op-tokens (EOF ADD SUBTRACT PRODUCT DIVISION POWER OP CP EQ NEG NEWLINE ))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ (:& (:~ #\newline) whitespace)) (return-without-pos (next-token input-port))] 
   [#\+ (token-ADD)]
   [#\- (token-SUBTRACT)]
   [#\* (token-PRODUCT)]
   [#\/ (token-DIVISION)]      
   [#\^ (token-POWER)]
   [#\( (token-OP)]
   [#\) (token-CP)]
   [#\= (token-EQ)]
   [#\newline (token-NEWLINE)]
   [(:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]
   [(:: alphabetic (:* (:or alphabetic numeric))) (let ([sym (string->symbol lexeme)])
                                                    (if (fun? sym)
                                                        (token-FUN sym)
                                                        (token-VAR sym)))]))                                                  
   

(provide value-tokens op-tokens next-token)   


