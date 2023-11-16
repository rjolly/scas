package scas.variable

import scas.prettyprint.Show

class Function[T : Show](name: String, parameter: T*) extends Variable {
  override def toString = s"$name(${parameter.toList.show(false)})"
  def toMathML = s"<apply><ci>${name.toMathML}</ci>${parameter.toList.toMathML(false)}</apply>"
}
