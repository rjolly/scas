package scas.variable

import scas.structure.Structure

class Sqrt[T : Structure](x: T) extends Function("sqrt", x) {
  override def toMathML = s"<apply><root/>${x.toMathML}</apply>"
}
