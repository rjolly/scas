package scas.variable

import Variable.toMathML

case class Constant(name: String, prime: Int, subscript: Int*) extends Variable {
  override def toString = name + (for (i <- 0 until prime) yield "I").mkString + subscript.map("(" + _ + ")").mkString
  def toMathML = if (prime == 0) (if (subscript.isEmpty) s"<ci>${name.toMathML}</ci>" else s"<ci><msub><mi>${name.toMathML}</mi><mrow>${subscript.map(a => s"<mn>$a</mn>")}</mrow></msub></ci>") else (if (subscript.isEmpty) s"<ci><msup><mi>${name.toMathML}</mi><mrow>${for (i <- 0 until prime) yield "<mo>I</mo>"}</mrow></msup></ci>" else s"<ci><msubsup><mi>${name.toMathML}</mi><mrow>${subscript.map(a => s"<mn>$a</mn>")}</mrow><mrow>${for (i <- 0 until prime) yield "<mo>I</mo>"}</mrow></msubsup></ci>")
}
