package de.hfu.topdown

import scala.collection.mutable

object Stack {
  def apply() = new Stack()

}

class Stack(private val frames: List[mutable.Map[String, Value]] = List(mutable.HashMap[String, Value]())) {

  def extend(frame: mutable.Map[String, Value]) =
    new Stack(frame :: frames)

  def apply(name: String): Option[Value] =
    frames.collectFirst({
      case map: Map[String, Value] if map.contains(name) => map(name)
    })


  // TODO are variables from upper frames allowed to be updated?
  def update(name: String, value: Value): Value = {
    frames.head(name) = value
    value
  }
}
