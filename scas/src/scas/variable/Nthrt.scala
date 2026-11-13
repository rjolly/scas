package scas.variable

import scas.prettyprint.Show

case class Nthrt[T : Show](x: T, n: T) extends Function("nthrt", x, n) {
  override val toMathML = s"<apply><root/>${x.toMathML}${n.toMathML}</apply>"
}
