Here will be formal language specification in [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form).

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
<pointterm>             ::= <factor> (( "*" | "/" ) <factor>)*
<factor>                ::= ("-"|"!")* <primary>
<primary>               ::= "(" <expression> ")"
                          | <function-call>
                          | <identifier>
                          | <int>
                          | <bool>


<function-call>         ::= <function> "(" <argument-list> ")"
<function>              ::= <function-literal>
                          | <identifier>
<function-literal>      ::= "fn (" <parameter-list> ")" <block-statement>
<block-statement>       ::= "{" <statement-list> "}"
<argument-list>         ::= { <expression> "," }
<parameter-list>        ::= { <identifier> "," }

<int>                   ::= <digit> { <digit> }
<digit>                 ::= "0..9"
<alpha>                 ::= "a..zA..Z"
<bool>                  ::= "true"
                          | "false"
```
