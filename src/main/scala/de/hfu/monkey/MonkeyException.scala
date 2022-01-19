package de.hfu.monkey

import de.hfu.monkey.lexer.Token

object MonkeyException {
  def apply(expected: Token, actual: Token) =
    new MonkeyException("I expected the token '" + expected + "' but found '" + actual + "'")

  def apply(expected: String, actual: Token) = new MonkeyException("I tried to read a " + expected + " but found the unexpected token '" + actual + "'")
}

case class MonkeyException(message: String) extends RuntimeException(message)

