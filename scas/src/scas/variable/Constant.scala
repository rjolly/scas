package scas.variable

import Variable.toMathML

class Constant(name: String, prime: Int, subscript: Int*) extends Variable {
  override def toString = name + (for (i <- 0 until prime) yield "I").mkString + subscript.map("(" + _ + ")").mkString
  def toMathML = s"<ci>${bodyToMathML}</ci>"
  def bodyToMathML = if (subscript.isEmpty) namePrimeToMathML else s"<msub><mi>${namePrimeToMathML}</mi><mrow>${subscriptToMathML}</mrow></msub>"
  def namePrimeToMathML = if (prime == 0) name.toMathML else s"${name.toMathML}${primecharsToMathML}"
  def primecharsToMathML = for (i <- 0 until prime) yield "\u2032"
  def subscriptToMathML = subscript.map(a => s"<mn>$a</mn>").mkString
}
