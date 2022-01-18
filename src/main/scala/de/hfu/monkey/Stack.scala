package de.hfu.monkey

import scala.collection.mutable

object Stack {
  def apply() = new Stack()

}

class Stack(private val frames: List[mutable.Map[String, Value]] = List(mutable.HashMap[String, Value]())) {

  def extend(frameData: List[(String, Value)]) =
    new Stack(collection.mutable.Map(frameData: _*) :: frames)

  def apply(name: String): Option[Value] =
    frames.map(_.get(name)).collectFirst({
      case Some(value) => return Some(value)
    })


  // TODO are variables from upper frames allowed to be updated?
  def update(name: String, value: Value): Value = {
    frames.head(name) = value
    value
  }
}
