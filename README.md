This a scala implementation of the [monkey programming language](https://monkeylang.org). Monkey is a simple C-style
language. An interpreter with Golang is explained in the
book [Writing An Interpreter In Go](https://interpreterbook.com/). The Scala's case classes offer a very simple approach
which is guided by the [EBNF](#EBNF) of Monkey.

Example: the line

```sh
<let-statement>         ::= "let" <identifier> "=" <expression> ";"
```

is mapped to

```sh
case class LetStatement(name: String, expression: Expression) 
```

In this implementation every case class has a compagnion object, which contains an apply method to parse a node in the
abstract syntax tree (AST):

```sh
object LetStatement {
  def apply(lexer: Lexer): LetStatement = {
    lexer.nextToken()
    val identifier = Identifier(lexer)
    lexer.expectCurrent(AssignmentToken)
    val expression = Expression(lexer)
    lexer.skipToken(SemicolonToken)
    LetStatement(identifier.value, expression)
  }
}
```

Parsing the code is also close to the EBNF of the particular node

## Installation

Install [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html)

Download this repository and change to the directory with this README file.

## Basic Use

To start the REPL (read-evalaute-print-loop) run

```sh
sbt run 
```

To execute monkey commands interactively.

To execute a file with monkey source code run

```sh
sbt "run examples/factorial.mon"
```

## EBNF

A formal definition of monkey does not exist. This is an attempt to map the language to EBNF

```
<program>               ::= <statement-list>
<statement-list>        :: { <statement> }
<statement>             ::= <let-statement>
                          | <return-statement>
                          | <expression-statement>
<let-statement>         ::= "let" <identifier> "=" <expression> ";"
<return-statement>      ::= "return" <expression> ";"
<expression-statement>  ::= <expression>  ";" 


<expression>            ::= <equality>
<equality>              ::= <comparison> [("=="|"!=") <comparison>]
<comparison>            ::= <dashterm> [("<"|">") <dashterm>]
<dashterm>              ::= <pointterm> (("-"|"!") <pointterm>)*
<pointterm>             ::= <unary> (( "*" | "/" ) <unary>)*
<unary>                ::= ("-"|"!")* call
<call>                  ::= primary [ "(" <argument-list> ")"] 
<primary>               ::= <paranthized-expression>
                          | <identifier>
                          | <int>
                          | <bool>
                          | <if-expression>
                          | <function-literal>



<paranthized-expression>::= "(" expression ")"
<function>              ::= <function-literal>
                          | <identifier>
<function-literal>      ::= "fn (" <parameter-list> ")" <block-statement>
<if-expression>         ::= "if" "(" <expression> ")" <block-statement> ["else" <block-statement>]
<block-statement>       ::= "{" <statement-list> "}"
<argument-list>         ::= { <expression> "," }
<parameter-list>        ::= { <identifier> "," }

<int>                   ::= <digit> { <digit> }
<digit>                 ::= "0..9"
<alpha>                 ::= "a..zA..Z"
<bool>                  ::= "true"
                          | "false"
```
