#lang racket

(provide get-fun fun? get-var set!-var )

(define funs
  (hasheq 'atan atan
          'cos cos
          'exp expt
          'ln log
          'sin sin
          'sqrt sqrt))

(define vars (make-hash))

(define (fun? key)
  (hash-has-key? funs key))
  
(define (var? key)
  (hash-has-key? vars key))

(define (get-fun key)
  (if (fun? key)
      (hash-ref funs key)
      (error "fun: no such function. " key)))
  
(define (get-var key)
  (if (var? key)
      (hash-ref vars key)
      (error "var: no such variable. " key)))

(define (set!-var key val)
  (hash-set! vars key val))


