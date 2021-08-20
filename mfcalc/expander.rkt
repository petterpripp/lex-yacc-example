#lang racket

(require (for-syntax syntax/parse) "funs.rkt")

(provide ;(except-out (all-from-out racket)  #%module-begin) 
 (rename-out [module-begin #%module-begin])
 #%top-interaction
 #%app
 #%datum
 quote
 add subtract product division power neg fun assign var)
 
         
(define-syntax (module-begin stx) 
  (syntax-parse stx
      [(module-begin expr ...)       
       #'(#%module-begin
          expr ...)]))
  

(define (add x y)
  (+ x y))

(define (subtract x y)
  (- x y))

(define (product x y)
  (* x y))

(define (division x y)
  (/ x y))

(define (power x y)
  (expt x y))

(define (neg x)
  (- x))

(define (fun name x)
  ((get-fun name) x))

(define (assign varname value)
  (begin
    (set!-var varname value)
    value))

(define (var name)
  (get-var name))