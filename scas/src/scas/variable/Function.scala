package scas.variable

import scas.prettyprint.Show
import Show.given

open class Function[T : Show](name: String, parameter: T*) extends Variable {
  override val toString = s"$name(${parameter.toList.show(false)})"
  val toMathML = s"<apply><ci>${name.toMathML}</ci>${parameter.toList.toMathML(false)}</apply>"
}
