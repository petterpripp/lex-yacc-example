#lang racket

(require parser-tools/yacc "lexer.rkt")
 

(define myparser
  (parser

   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens )
   (src-pos)
   (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   

   (precs
    (left ADD SUBTRACT)
    (left PRODUCT DIVISION)
    (nonassoc NEG)
    (left POWER))
    
   
   (grammar

    (exp  [(NUMBER) $1]
          [(exp ADD exp) (+ $1 $3)]
          [(exp SUBTRACT exp) (- $1 $3)]
          [(exp PRODUCT exp) (* $1 $3)]
          [(exp DIVISION exp) (/ $1 $3)]
          [(SUBTRACT exp) (prec NEG) (- $2)]
          [(exp POWER exp) (expt $1 $3)]))))
          
             
(define (parse ip)
  (port-count-lines! ip)
  (myparser (lambda () (next-token ip))))
    

(provide parse )
