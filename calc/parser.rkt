#lang racket

(require parser-tools/yacc "lexer.rkt")
 

(define myparser
  (parser

   (start input)
   (end EOF)
   (tokens value-tokens op-tokens )
   (src-pos)
   (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   

   (precs
    (left ADD SUBTRACT)
    (left PRODUCT DIVISION)
    (nonassoc NEG)
    (right POWER))    
   
   (grammar

    (input [() '()]           
           [(input line) (append $1  $2)])    

    (line [(NEWLINE) '()]
          [(exp NEWLINE) (list $1)])

    (exp  [(NUMBER) $1]
          [(exp ADD exp) (+ $1 $3)]
          [(exp SUBTRACT exp) (- $1 $3)]
          [(exp PRODUCT exp) (* $1 $3)]
          [(exp DIVISION exp) (/ $1 $3)]
          [(SUBTRACT exp) (prec NEG) (- $2)]
          [(exp POWER exp) (expt $1 $3)]
          [(OP exp CP) $2]))))
          
             
(define (parse ip)
  (port-count-lines! ip)
  (myparser (lambda () (next-token ip))))
    

(provide parse )
