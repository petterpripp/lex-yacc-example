#lang info

(define collection  "lex-yacc-example")
(define deps '("base" "parser-tools-lib" "sandbox-lib"))               
(define build-deps '("racket-doc" "scribble-lib" "parser-tools-doc" ))
(define scribblings '(("scribblings/lex-yacc.scrbl" ())))
(define pkg-desc "Examples for lexer and yacc")
(define pkg-authors '(Petter Pripp))
(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))
