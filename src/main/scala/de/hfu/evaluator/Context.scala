package de.hfu.evaluator

import scala.collection.mutable

object Context {
  def apply() = new Context()

}

class Context private(private val variables: mutable.Map[String, Value] = mutable.HashMap[String, Value](), private val outerContext: Option[Context] = None) {


  def extend(extendedVariables: mutable.Map[String, Value]) = new Context(extendedVariables, Some(this))

  def apply(name: String): Option[Value] = outerContext match {
    case None => variables.get(name)
    case Some(context) => context(name) match {
      case None => variables.get(name)
      case value => value
    }
  }

  def update(name: String, value: Value): Value = {
    variables(name) = value
    value
  }
}
