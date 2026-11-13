package scas.variable

import scas.prettyprint.Show

case class Pow[T : Show](x: T, y: T) extends Function("pow", x, y) {
  override val toMathML = s"<apply><power/>${x.toMathML}${y.toMathML}</apply>"
}
