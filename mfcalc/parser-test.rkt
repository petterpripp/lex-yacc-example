#lang racket

(require "parser.rkt")

;(parse (open-input-string "x = 10 \n  x + 5 \n x *ln(2) \n"))

(parse (open-input-string "1 + 2\n 3 * 4 \n 5 * 6 + 7\n (1 + 2) * 3\n \n x = 10\n "))