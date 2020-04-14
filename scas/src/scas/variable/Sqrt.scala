package scas.variable

import scas.structure.Structure

case class Sqrt[T : Structure](x: T) extends FunctionImpl("sqrt", x) {
  override def toMathML = s"<apply><root/>${x.show.toMathML}</apply>"
}
