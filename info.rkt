#lang info

(define collection  'multi)
(define deps '("base" ))               
(define build-deps '("racket-doc" "scribble-lib" ))
(define scribblings '(("scribblings/lex-yacc.scrbl" ())))
(define pkg-desc "Examples for lexer and yacc")
(define pkg-authors '(Petter Pripp))
(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))
