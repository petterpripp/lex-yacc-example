#lang scribble/manual

@(require scribble/eval racket/sandbox  scribble/bnf racket/port  racket/string
          (for-label (except-in racket exp error)  parser-tools/lex))

@title{Lexer and yacc tutorial}
by @author+email[ "Petter Olav Pripp" "petter.pripp@yahoo.com"]

Copyright (C) 2021 - Petter Olav Pripp

The source code is distributed under the GNU General Public License.

The source code is at

@url{https://github.com/petterpripp/lex-yacc-example}

Any suggestion or corrections are welcome.

@(define dir (string-trim (path->string (collection-file-path " " "lex-yacc-example"))))

@(define xeval (make-base-eval))
@interaction-eval[#:eval xeval  (require lex-yacc-example/rpcalc/lexer-test  lex-yacc-example/rpcalc/parser ) ]

@(define calc-eval (make-base-eval))
@interaction-eval[#:eval calc-eval  (require lex-yacc-example/calc/lexer-test  lex-yacc-example/calc/parser ) ]

@define[(kode filnavn)
        ;(define dir (string-trim (path->string (collection-file-path " " "lex-yacc-example"))))
        (codeblock (string-trim(port->string (open-input-file (string-append dir filnavn)))))]


@section{Reverse Polish Notation Calculator}

Based on GNU Bison RPN Calc example.

@url{https://www.gnu.org/software/bison/manual/bison.html#RPN-Calc}

The example is that of a simple double-precision Reverse Polish Notation calculator (a calculator using postfix operators).
This example provides a good starting point, since operator precedence is not an issue.

@subsection{BNF}

@BNF[(list @nonterm{exp}
           @nonterm{number}
           @BNF-seq[@nonterm{exp} @nonterm{exp} @litchar{+}]
           @BNF-seq[@nonterm{exp} @nonterm{exp} @litchar{-}]
           @BNF-seq[@nonterm{exp} @nonterm{exp} @litchar{*}]
           @BNF-seq[@nonterm{exp} @nonterm{exp} @litchar{/}]
           @BNF-seq[@nonterm{exp} @nonterm{exp} @litchar{^}]
           @BNF-seq[@nonterm{exp} @litchar{-}])]

@subsection{Lexer}

The lexer translate code to tokens. This will be input to the parser.
Below is the full code for the lexer. In the next sections we will look into the code.


@kode{rpcalc/lexer.rkt} 
@; @codeblock[(string-trim(port->string (open-input-file "../rpcalc/lexer.rkt")))]

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

@kode{rpcalc/lexer-test.rkt} 


@examples[
 #:eval  xeval
 (my-lex-test "1 + * -")
 (my-lex-test "3.14 +\n ^ # -") ]
 

@subsection{Parser}

This is the full code for the parser. In the next sections we will look into the code.

@kode{rpcalc/parser.rkt} 

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

@subsection{Language}

We wrap it up with making a rpcalc language.

@subsubsection{Reader}

@kode{rpcalc/reader.rkt} 


@subsubsection{Main}

@kode{rpcalc/main.rkt} 


@subsubsection{Running rpcalc}

@codeblock{
           
#lang lex-yacc-example/rpcalc

2 3 4 5 + + ^ n

}          

The result should be @code{-4096}.


@section{Infix Notation Calculator}

Based on GNU Bison Infix Calc example.

@url{https://www.gnu.org/software/bison/manual/bison.html#Infix-Calc}


We now modify rpcalc to handle infix operators instead of postfix.
Infix notation involves the concept of operator precedence and the need for parentheses nested to arbitrary depth. 

@subsection[#:tag "calcbnf" "BNF"]

@BNF[(list @nonterm{input}
           @BNF-seq[@nonterm{input} @nonterm{line}])
     (list @nonterm{line}
           @BNF-seq[@litchar{\n}]
           @BNF-seq[@nonterm{exp} @litchar{\n}])
     (list @nonterm{exp}
           @nonterm{number}                                 
           @BNF-seq[@nonterm{exp} @litchar{+} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{-} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{*} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{/} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{^} @nonterm{exp} ]
           @BNF-seq[@litchar{-}  @nonterm{exp}]
           @BNF-seq[@litchar{(} @nonterm{exp} @litchar{)}])]
     


@subsection[#:tag "calclexer" "Lexer"]
Below is the full code for the lexer.

@kode{calc/lexer.rkt}
@; codeblock[(port->string (open-input-file "../calc/lexer.rkt"))]

Changes from rpcalc:
Newline is a token, whitespace without newline, and token for '(' and ')'.
Neg will not be used in lexer, but is defined because of use in parser later on.


@subsection[#:tag "calcparser" "Parser"]
Below is the full code for the parser.

@kode{calc/parser.rkt}

There are two important new features shown in this code.

In the precs section, left declares token kinds and says they are left-associative operators.
And right (right associativity).

Operator precedence is determined by the line ordering of the declarations.
The higher the line number of the declaration (lower on the page or screen), the higher the precedence.
Hence, exponentiation has the highest precedence, unary minus (NEG) is next, followed by ‘*’ and ‘/’, and so on.
Unary minus is not associative, only precedence matters.

The other important new feature is the prec in the grammar section for the unary minus operator.
The prec simply instructs Yacc that the rule @racket[(SUBTRACT exp)] has the same precedence as NEG. In this case the next-to-highest. 

@subsubsection[#:tag "calcparsertest" "Testing the parser"]

@examples[
 #:eval  calc-eval
 (parse (open-input-string "\n\n1 + 4*8 \n 6/10\n\n\n 5 + 6 +7 \n\n\n"))
 (parse (open-input-string "\n\n(1 + 4)*8 \n 6/10\n\n\n 5 + 6 +7 \n\n\n"))
 (parse (open-input-string "1 + 2^4 \n"))]


@subsection[#:tag "calclanguage" "Language"]

We wrap it up with making a calc language.

@subsubsection[#:tag "calcreader" "Reader"]
@kode{calc/reader.rkt} 

Note the quote at:  @code{',(parse port)}

@subsubsection[#:tag "calcmain" "Main"]
@kode{calc/main.rkt} 

@subsubsection{Running calc}

@codeblock{

#lang lex-yacc-example/calc

1 + 4 * 8

(1 + 4) * 8

6/10

2 ^ 4 + 100 

} 


The result should be @racket['(33 40 3/5 116)].

@section{Multi-Function Calculator}

Based on GNU Bison Multi-Function Calc example.

@url{https://www.gnu.org/software/bison/manual/bison.html#Multi_002dfunction-Calc}


Now that the basics of lexer and yacc have been discussed, it is time to move on to a more advanced problem.
The above calculators provided only five functions, ‘+’, ‘-’, ‘*’, ‘/’ and ‘^’.
It would be nice to have a calculator that provides other mathematical functions such as sin, cos, etc.

It is easy to add new operators to the infix calculator as long as they are only single-character literals.
The lexer passes back all nonnumeric characters as tokens, so new grammar rules suffice for adding a new operator.
But we want something more flexible: built-in functions whose syntax has this form: 
@codeblock{function_name (argument)}
At the same time, we will add memory to the calculator, by allowing you to create named variables,
store values in them, and use them later.

@subsection[#:tag "mfcalcbnf" "BNF"]

@BNF[(list @nonterm{input}
           @BNF-seq[@nonterm{input} @nonterm{line}])
     (list @nonterm{line}
           @BNF-seq[@litchar{\n}]
           @BNF-seq[@nonterm{exp} @litchar{\n}]
           @BNF-seq[@nonterm{var} @litchar{=} @nonterm{exp} @litchar{\n}])
     (list @nonterm{exp}
           @nonterm{number}
           @nonterm{var}           
           @BNF-seq[@nonterm{fun} @litchar{(} @nonterm{exp} @litchar{)}]
           @BNF-seq[@nonterm{exp} @litchar{+} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{-} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{*} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{/} @nonterm{exp}]
           @BNF-seq[@nonterm{exp} @litchar{^} @nonterm{exp} ]
           @BNF-seq[@litchar{-}  @nonterm{exp}]
           @BNF-seq[@litchar{(} @nonterm{exp} @litchar{)}])]

@subsection[#:tag "mfcalclexer" "Lexer"]
Below is the full code for the lexer.

@kode{mfcalc/lexer.rkt}


The lexer has to decide between FUN or VAR token. This is done by query if function is defined.
The function @code{fun?} gives the answer. More about function and variable in next section.

@subsection[#:tag "funs" "funs.rkt"]

The new file where function and variable is handled.

@kode{mfcalc/funs.rkt}

Function is stored in immutable hash-table.

Variable is stored in mutable hash-table, the memory where variable's is defined and changed.

@subsection[#:tag "mfcalcparser" "Parser"]
Below is the full code for the parser.

@kode{mfcalc/parser.rkt}

The parser generate s-expression's for input to the expander. This is an important step forward.
When program's becomes more complicated it is better to handle this separate from the parser.
In Racket it is common that parsing is in the reader and runnable code is in the expander.


@subsection[#:tag "mfcalclanguage" "Language"]

We will making a mfcalc language, using reader and expander.


@subsubsection[#:tag "mfcalcexpander" "Expander"]
@kode{mfcalc/expander.rkt} 

@subsubsection[#:tag "mfs-exp-test" "Testing s-exp"]

The s-exp version of mfcalc can be tested by using the @code{#lang s-exp} declaration

@kode{mfcalc/s-exp-test.rkt} 


@subsubsection[#:tag "mfcalcreader" "Reader"]

The reader module are using "expander.rkt".

@kode{mfcalc/reader.rkt} 


@subsubsection[#:tag "mfcalcmain" "Main"]
@kode{mfcalc/main.rkt} 


@section{Conclusion}

Congratulation! You are now a lexer and yacc ninja!

Some other sources:

The GNU Bison manual covers many topics of yacc/bison parser. Useful even if you can not program in C.

@url{https://www.gnu.org/software/bison/manual/bison.html}

The racket parser have two examples: calc.rkt and read.rkt

@url{https://github.com/racket/parser-tools/tree/master/parser-tools-lib/parser-tools/examples}
