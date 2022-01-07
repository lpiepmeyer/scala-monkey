package de.hfu.evaluator

class Context {
  private val variables = scala.collection.mutable.HashMap[String, Value]()

  def apply(name: String): Value = variables(name)

  def update(name: String, value: Value): Value = {
    variables(name) = value
    value
  }
}
