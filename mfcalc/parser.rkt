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
    (left POWER))
   
   (grammar

    (input [() '()]
           [(input line) (append $1 $2)])

    (line [(NEWLINE) '()]
          [(exp NEWLINE) (list $1)]
          [(VAR EQ exp NEWLINE)  `((assign ',$1 ,$3))])

    (exp  [(NUMBER) $1]
          [(VAR) `(var ',$1)]          
          [(FUN OP exp CP) `(fun ',$1 ,$3)]
          [(exp ADD exp) `(add ,$1 ,$3)]
          [(exp SUBTRACT exp) `(subtract ,$1 ,$3)]
          [(exp PRODUCT exp) `(product ,$1 ,$3)]
          [(exp DIVISION exp) `(division ,$1 ,$3)]
          [(SUBTRACT exp) (prec NEG) `(neg ,$2)]
          [(exp POWER exp) `(power ,$1 ,$3)]
          [(OP exp CP) $2]))))
          
             
(define (parse ip)
  (port-count-lines! ip)  
  (myparser (lambda () (next-token ip))))
    

(provide parse )
