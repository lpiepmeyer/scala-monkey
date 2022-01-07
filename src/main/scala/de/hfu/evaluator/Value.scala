package de.hfu.evaluator

abstract class Value

case class BooleanValue(v: Boolean) extends Value

case class IntegerValue(v: Int) extends Value

case object NoValue extends Value

case class ReturnValue(value: Value) extends Value
