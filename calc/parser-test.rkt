#lang racket

(require "parser.rkt")


(parse (open-input-string "4^2+5-6 * 10"))

(parse (open-input-string "-3 + 10"))

