package scas.variable

import scas.structure.Structure
import Variable.toMathML

class Function[T : Structure](name: String, parameter: T*) extends Variable {
  override def toString = name + "(" + parameter.map(_.show).mkString(", ") + ")"
  def toMathML = s"<apply><ci>${name.toMathML}</ci>${parameter.map(_.toMathML)}</apply>"
}
