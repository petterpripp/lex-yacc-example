#lang racket

(require "parser.rkt")

(parse (open-input-string "\n\n1 + 4*8 \n 6/10\n\n\n 5 + 6 +7 \n\n\n"))

(parse (open-input-string "\n\n(1 + 4)*8 \n 6/10\n\n\n 5 + 6 +7 \n\n\n"))


