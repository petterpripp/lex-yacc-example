#lang racket

(require rackunit rackunit/text-ui
         (prefix-in r: lex-yacc-example/rpcalc/reader)
         (prefix-in c: lex-yacc-example/calc/reader)
         (prefix-in mp: lex-yacc-example/mfcalc/parser))


(run-tests
 
 (test-suite
  "lex-yacc-example"

  (test-case
   "rpcalc + *"
   (check-equal?
    (last (r:read (open-input-string "2 3 7 + *")))
    20))

  (test-case
   "rpcalc test example"
   (check-equal?
    (last (r:read (open-input-string "2 3 4 5 + + ^ n")))
    -4096))

  (test-case
   "calc test example"
   (check-equal?
    (last (c:read (open-input-string "1 + 4 * 8\n(1 + 4) * 8\n6/10\n2 ^ 4 + 100\n")))
    ''(33 40 3/5 116)))

  (test-case
   "mfcalc parse"
   (check-equal?
    (mp:parse (open-input-string "x = 5\n x + 10\n "))
    '((assign 'x 5) (add (var 'x) 10))))

  ))
