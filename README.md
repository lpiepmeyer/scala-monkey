# A Scala Implementation of Monkey Programming Language

This a scala implementation of the [monkey programming language](https://monkeylang.org). Monkey is a simple C-style
language. An interpreter with Golang is explained in the
book [Writing An Interpreter In Go](https://interpreterbook.com/). Scala's case classes offer a very simple approach
which is guided by the [EBNF](#EBNF) of Monkey.

On my laptop, that takes jlox about 72 seconds to execute. An equivalent C program finishes in half a second 68.60s fib(
35); 0.03s c-prgramm mot -O3

### Example

The line

```sh
<let-statement>         ::= "let" <identifier> "=" <expression> [";"]
```

is mapped to the scala class

```sh
case class LetStatement(identifier: Identifier, expression: Expression) 
```

Every case class in this implementation has a compagnion object, which contains an `apply`-method to parse a node for
the correpsonding abstract syntax tree (AST).Parsing the code is again similar to the EBNF of the particular node

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

Every case class contains an `evaluate`-method to evaluate the AST during interpreation. The `evaluate`-method for the
let-statement is

```sh
 override def evaluate(stack: Stack): Value = stack(identifier.value) = expression.evaluate(stack)
```

Where `stack` is the context of the monkey program, containing values for all variables.

## Installation

* Install [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html)

* Download this repository and change to the directory with this README file.

## Basic Use

To start the REPL (read-evalaute-print-loop) run

```sh
sbt run 
```

To execute monkey commands interactively:

```sh
starting REPL
>> let a=23
23
>> let a=a+19
42
>> exit
terminating REPL
```

To execute a file with monkey source code run

```sh
sbt "run examples/factorial.mon"
720
```

## Examples

The examples folder contains examples which can be run by interpreter. The last line contains a comment with the
expected result of the programm. This reuslts are used by the test `InterpreterTest` in
the `src/test/scala/de/hfu/monkey` directory.

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
