package de.hfu.lexer

object Precedence extends Enumeration {
  type Precedence = Value
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL = Value
}
