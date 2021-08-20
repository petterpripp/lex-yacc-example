#lang s-exp "expander.rkt"

(add 1 2)
(subtract (add 3 4) 5)
(product (power 2 4) 10)
(division (product (power 2 4) 10) 5)
(product 100 (fun 'sqrt 25))
(assign 'x 50)
(division (var 'x) (add 3 7))
(assign 'y 20)
(add (var 'x) (var 'y))
(assign 'x 60)
(add (var 'x) (var 'y))