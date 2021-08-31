#lang racket

(require parser-tools/yacc  "lexer.rkt")

(define myparser
  (parser

   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens )
   (src-pos)
   (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   
   
   (grammar

    (exp  [(NUMBER) $1]
          [(exp exp ADD) (+ $1 $2)]
          [(exp exp SUBTRACT) (- $1 $2)]
          [(exp exp PRODUCT) (* $1 $2)]
          [(exp exp DIVISION) (/ $1 $2)]
          [(exp exp POWER) (expt $1 $2)]
          [(exp NEG) (- $1)]))))
             
(define (parse ip)
  (port-count-lines! ip)  
  (myparser (lambda () (next-token ip))))   

(provide parse )
