package scas.variable

import scas.prettyprint.Show

class Sqrt[T : Show](x: T) extends Function("sqrt", x) {
  override val toMathML = s"<apply><root/>${x.toMathML}</apply>"
}
