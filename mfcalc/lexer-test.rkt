#lang racket

(require "lexer.rkt"  parser-tools/lex)


(define (lex-test ip)
  (port-count-lines! ip)
  (letrec ([one-line
	    (lambda ()
	      (let ([result (next-token ip)])
                (unless (equal?	(position-token-token result) 'EOF)
                  (printf "~a\n" result)
                  (one-line)
                  )))])
    (one-line)))


(lex-test (open-input-string "3.14 +   \t \n - / ^  sin cos34 log(1)"))
