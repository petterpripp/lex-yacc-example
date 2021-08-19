#lang scribble/manual

@(require scribble/eval racket/sandbox (for-label (except-in racket exp error)  parser-tools/lex))

@title{Lexer and yacc tutorial}
by @author+email[ "Petter Olav Pripp" "petter.pripp@yahoo.com"]

Copyright (C) 2021 - Petter Olav Pripp

The tutorial and source code is distributed under the GNU General Public License.

The source code is at

@url{https://github.com/petterpripp/tutorial}

Any suggestion or corrections are welcome.

@(define xeval (make-base-eval))
@interaction-eval[#:eval xeval  (require "../rpcalc/lexer-test.rkt"  "../rpcalc/parser.rkt" ) ]  

@section{Reverse Polish Notation Calculator}

Based on GNU Bison RPN Calc example.

@url{https://www.gnu.org/software/bison/manual/bison.html#RPN-Calc}

The example is that of a simple double-precision Reverse Polish Notation calculator (a calculator using postfix operators).
This example provides a good starting point, since operator precedence is not an issue.



@subsection{Lexer}

The lexer translate code to tokens. This will be input to the parser.
Below is the full code for the lexer. In the next sections we will look into the code.

@codeblock|{
#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


(define-tokens value-tokens (NUMBER))
(define-empty-tokens op-tokens (EOF ADD SUBTRACT PRODUCT DIVISION POWER NEG))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace) (return-without-pos (next-token input-port))]
   [#\+ (token-ADD)]
   [#\- (token-SUBTRACT)]
   [#\* (token-PRODUCT)]
   [#\/ (token-DIVISION)]      
   [#\^ (token-POWER)]
   [#\n (token-NEG)]
   [(:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]))
 

(provide value-tokens op-tokens next-token)   
}|             

@subsubsection{Two types of tokens}

@racketblock[
(define-tokens value-tokens (NUMBER))
(define-empty-tokens op-tokens (EOF ADD SUBTRACT PRODUCT DIVISION POWER NEG))]

@itemlist[
 @item{Value tokens combines the token-id and the value.}
 @item{Empty tokens is only token-id.}]


@subsubsection{lexer-src-pos }

The lexer uses regular expressions from  @code{(require parser-tools/lex (prefix-in : parser-tools/lex-sre))}.
When multiple patterns match, a lexer will choose the longest match, breaking ties in favor of the rule appearing first.
The lexer will return tokens with source information. Below is explanation of some of the rules.

@racketblock[
 [#\+ (token-ADD)]]

When lexer finds '+' it will return token ADD.

@racketblock[
[(:+ whitespace) (return-without-pos (next-token input-port))]]             

Recursively call the lexer on the remaining input after a tab or space.
Returning the result of that operation. This effectively skips all whitespace.   

@racketblock[
[(:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]]

The lexer return both token-id NUMBER and the number combined to one value-token.


@subsubsection{Testing the lexer}

@codeblock|{
(require "lexer.rkt" parser-tools/lex)

(define (lex-test ip)
  (port-count-lines! ip)
  (letrec ([one-line
            (lambda ()
              (let ([result (next-token ip)])
                (unless (equal?	(position-token-token result) 'EOF)
                  (printf "~a\n" result)
                  (one-line)
                  )))])
    (one-line)))

(define (my-lex-test str)
    (lex-test (open-input-string str)))
    
(provide my-lex-test)
}|

@examples[
#:eval  xeval
(my-lex-test "1 + * -")
(my-lex-test "3.14 +\n ^ # -") ]
 
 


@subsection{Parser}

This is the full code for the parser. In the next sections we will look into the code.

@codeblock|{
#lang racket
(require parser-tools/yacc  "lexer.rkt")
 
(define myparser
  (parser

   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens )
   (src-pos)
   (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   
   
   (grammar

    (exp  [(NUMBER) $1]
          [(exp exp ADD) (+ $1 $2)]
          [(exp exp SUBTRACT) (- $1 $2)]
          [(exp exp PRODUCT) (* $1 $2)]
          [(exp exp DIVISION) (/ $1 $2)]
          [(exp exp POWER) (expt $1 $2)]
          [(exp NEG) (- $1)]))))

             
(define (parse ip)
  (port-count-lines! ip)  
  (myparser (lambda () (next-token ip))))    

(provide parse )
}|             

@subsubsection{Explanation of exp grammar }

Grammar can have many grouping. In this example it has only exp.
The exp grouping has several rules, one for each kind of expression.
The first rule handles the simplest expressions: those that are just numbers.
The second handles an addition-expression, which looks like two expressions followed by a plus-sign.
The third handles subtraction, and so on. 

@racketblock[

 (grammar
  (exp  [(NUMBER) $1]
        [(exp exp ADD) (+ $1 $2)]
        [(exp exp SUBTRACT) (- $1 $2)]
        [(exp exp PRODUCT) (* $1 $2)]
        [(exp exp DIVISION) (/ $1 $2)]
        [(exp exp POWER) (expt $1 $2)]
        [(exp NEG) (- $1)]))]

The rules have actions that compute the value of the expression in terms of the value of its parts.
For example, in the rule for addition, $1 refers to the first component exp and $2 refers to the second one.


@subsubsection{The other components of parser }

@racketblock[(start exp)]
Starting point for the parser.
In our example there is only one grouping exp, therefore exp will be the starting grouping.

@racketblock[(end EOF)]
End point for the parser.
The parser will stop when it reads token EOF. 

@racketblock[(tokens value-tokens op-tokens )]
Declares all of the tokens that can be used by the parser in the grammar declaration.
Tokens is defined in the lexer.

@racketblock[(src-pos)]
Causes the generated parser to expect input in the form of token with source information, instead of only tokens.

@racketblock[ (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void)))) ]
The function which will be executed for its side-effect whenever the parser encounters an error.


@subsubsection{parse function }

@racketblock[
 (define (parse ip)
  (port-count-lines! ip)
  (myparser (lambda () (next-token ip))))]


Wrapper around the parser. It handles the call to the lexer.

@racketblock[(port-count-lines! ip)]
Necessary for having source code information in error message.

@subsubsection{Testing the parser}

@examples[
#:eval  xeval
(parse (open-input-string "20 3 5 * 7 + + "))
(parse (open-input-string "2 4 ^ n "))
(parse (open-input-string "1 2 3 / + * "))
]

@subsection{rpcalc language}

We wrap it up with making a rpcalc language.

@subsubsection{reader.rkt}

@codeblock|{

#lang racket

(require "parser.rkt" )
 
(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))
 
(define (my-read in)
  (syntax->datum
   (my-read-syntax #f in)))

(define (my-read-syntax path port)
  (datum->syntax
   #f
   `(module rpcalc-mod racket
      ,(parse port))))

}|


@subsubsection{main.rkt}

@codeblock|{
#lang racket

(module reader racket
  (require "reader.rkt")
  (provide read read-syntax)) }|

@subsubsection{Installing and running rpcalc language}

Open terminal and go to rpcalc directory. Run commando:

@bold{raco pkg install}


@subsubsection{Running rpcalc}

@codeblock|{
#lang rpcalc

2 3 4 5 + + ^ n 
}|            

The result should be @code{-4096}.


@subsection{Conclusion}

Congratulation! You are now a lexer and yacc ninja!

However this tutorial is far from complete.
Some other sources:

The GNU Bison manual covers many topics of yacc/bison parser. Useful even if you can not program in C.

@url{https://www.gnu.org/software/bison/manual/bison.html}

The racket parser have two examples: calc.rkt and read.rkt

@url{https://github.com/racket/parser-tools/tree/master/parser-tools-lib/parser-tools/examples}


