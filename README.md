# A Scala Implementation of the Monkey Programming Language

This a Scala implementation of the [Monkey programming language](https://monkeylang.org). Monkey is a simple C-style
language. An interpreter with Golang is explained in the book [Writing An Interpreter In Go]
(https://interpreterbook.com/).

### Scala Monkey is not fast

Consider the following piece of code:

```sh
let fibonacci = fn(n) {
  if (n < 2) {
    return n
  }
  fibonacci(n - 1) + fibonacci(n - 2)
}

fibonacci(35)
```

On my laptop, the execution with Scala Monkey took about 72 seconds. An equivalent C program with optimization level 3
finishes in 0.03s.

### Monkey is a simple, not a fancy programming language

This implementation covers the basic syntax of Monkey. This means no loops, no arrays, and only `int` and `bool`as data
types. Loops can be replaced by recursion:

```sh
let factorial = fn factorial n) {
  if (n < 1) { 1 }
  else { n * factorial(n - 1) }
};
```

Functions are first class citiziens in Monkey. A for loop may therefore be replaced by the following higher-order
function:

```sh
let for = fn(from, to, body, accu) {
  if (from > to) {
    return  accu
  }
  for(from+1, to, body, body(accu,from))
}
```

and factorial can be computed the follwing way:

```sh
for(1,5,fn(a,b){a*b},1)
```

The book explains how extensions for strings, arrays, and dictionaries can be added.

### Why Scala?

Scala's case classes offer a very simple approach which is guided by the [EBNF](#EBNF) of Monkey.

The line

```sh
<let-statement> ::= "let" <identifier> "=" <expression> [";"]
```

is mapped to the Scala class

```sh
case class LetStatement(identifier: Identifier, expression: Expression) 
```

Every case class in this implementation has a companion object, which contains an `apply`-method to parse a node for the
correpsonding abstract syntax tree (AST). Parsing the code is again similar to the EBNF of the particular node:

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

Every case class contains an `evaluate`-method to evaluate the AST during interpreation. The `evaluate`-method for
the ```let```-statement is

```sh
 override def evaluate(stack: Stack): Value = 
   stack(identifier.value) = expression.evaluate(stack)
```

Where `stack` is the context of the Monkey program, containing values of all variables.

## Installation

* Install the Scala Build Tool [(sbt)](https://www.scala-sbt.org/1.x/docs/Setup.html)

* Download this repository and change to the directory with this README file.

## Usage

There two ways to work with Scala Monkey:

1. Start the REPL (read-evalaute-print-loop) run:

```sh
sbt run 
```

To execute Monkey commands interactively:

```sh
starting REPL
>> let a=23
23
>> let a=a+19
42
>> exit
terminating REPL
```

2. Execute a file with Monkey source code:

```sh
sbt "run examples/factorial.mon"
720
```

## Examples

The examples folder contains examples which can be run by the interpreter. The last line contains a comment with the
expected result of the programm. The results are used by the test `InterpreterTest` in
the `src/test/scala/de/hfu/monkey` directory.

## EBNF

A formal definition of Monkey does not seem to exist. This is an attempt of an EBNF:

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
