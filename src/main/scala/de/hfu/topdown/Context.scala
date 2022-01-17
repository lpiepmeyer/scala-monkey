package de.hfu.topdown

import scala.collection.mutable

object Context {
  def apply() = new Context()

}

class Context private(private val variables: mutable.Map[String, Value] = mutable.HashMap[String, Value](), private val outerContext: Option[Context] = None) {


  def extend(extendedVariables: mutable.Map[String, Value]) = new Context(extendedVariables, Some(this))

  def apply(name: String): Option[Value] = variables.get(name) match {
    case Some(value) => Some(value)
    case None => outerContext match {
      case None => None
      case Some(context) => context(name)
    }
  }

  def update(name: String, value: Value): Value = {
    variables(name) = value
    value
  }
}
