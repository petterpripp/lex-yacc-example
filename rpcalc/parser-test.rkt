#lang racket

(require "parser.rkt" )


(parse (open-input-string "20 3 5 * 7 + + "))

