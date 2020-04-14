package scas.variable

import scas.structure.Structure
import Variable.toMathML

abstract class FunctionImpl[T : Structure](name: String, parameter: T*) extends Variable {
  override def toString = name + "(" + parameter.map(_.show).mkString(", ") + ")"
  def toMathML = s"<apply><ci>${name.toMathML}</ci>${parameter.map(_.show.toMathML)}</apply>"
}

case class Function[T : Structure](name: String, parameter: T*) extends FunctionImpl(name, parameter: _*)
