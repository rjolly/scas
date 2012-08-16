package scas.variable

class Constant(val name: String, val prime: Int, val subscript: Array[Int]) extends Variable {
  override def equals(other: Any) = other match {
    case other: Constant => {
      if (!this.name.equals(other.name)) false else {
        if (!this.prime.equals(other.prime)) false else (true /: this.subscript.zip(other.subscript)) { case (l, (a, b)) =>
          l && a.equals(b)
        }
      }
    }
  }
  override def hashCode = (name.hashCode /: (Array(prime)++subscript)) {
    (l, r) => l * 31 + r
  }
  override def toString = name + (for (i <- 0 until prime) yield "I").mkString + subscript.map("(" + _ + ")").mkString
  def toMathML = if (prime == 0) (if (subscript.length == 0) <ci>{mml(name)}</ci> else <ci><msub><mi>{mml(name)}</mi><mrow>{subscript.map(a => <mn>{a}</mn>)}</mrow></msub></ci>) else (if (subscript.length == 0) <ci><msup><mi>{mml(name)}</mi><mrow>{for (i <- 0 until prime) yield <mo>I</mo>}</mrow></msup></ci> else <ci><msubsup><mi>{mml(name)}</mi><mrow>{subscript.map(a => <mn>{a}</mn>)}</mrow><mrow>{for (i <- 0 until prime) yield <mo>I</mo>}</mrow></msubsup></ci>)
}
