package scas.variable

class Constant(name: String, prime: Int, subscript: Int*) extends Variable {
  override def toString = name + (for i <- 0 until prime yield "_").mkString + subscript.map("(" + _ + ")").mkString
  def toMathML = s"<ci>${bodyToMathML}</ci>"
  def bodyToMathML = if subscript.isEmpty then {
    if prime == 0 then name.toMathML
    else s"<msup><mi>${name.toMathML}</mi><mrow>${primeToMathML}</mrow></msup>"
  } else {
    if prime == 0 then s"<msub><mi>${name.toMathML}</mi><mrow>${subscriptToMathML}</mrow></msub>"
    else s"<msubsup><mi>${name.toMathML}</mi><mrow>${subscriptToMathML}</mrow><mrow>${primeToMathML}</mrow></msubsup>"
  }
  def primeToMathML = (for i <- 0 until prime yield "<mo>&#x02032;</mo>").mkString
  def subscriptToMathML = subscript.map(a => s"<mn>$a</mn>").mkString
}
