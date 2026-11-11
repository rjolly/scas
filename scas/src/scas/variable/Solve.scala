package scas.variable

import scas.prettyprint.Show

case class Solve[T : Show](equ: T, x: T, n: T) extends Function("solve", equ, x, n) {
  override val toMathML = s"<apply><ci>solve</ci>${equ.toMathML}${x.toMathML}${n.toMathML}</apply>"
}
