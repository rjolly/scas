package scas

import scala.math.Ordering.{Int, Iterable, String}

class Variable(val name: String, val prime: Int, val subscript: Array[Int]) extends Ordered[Variable] with MathObject {
  def compare (that: Variable) = Variable.compare(this, that)
  override lazy val hashCode = (name.hashCode /: (Array(prime)++subscript)) {
    (l, r) => l * 31 + r
  }
  override def equals(other: Any) = other match {
    case other: Variable => this.compare(other) == 0
    case _ => super.equals(other)
  }
  override def toString = name + (for (i <- 0 until prime) yield "I").mkString + subscript.map("(" + _ + ")").mkString
  def toMathML = if (prime == 0) (if (subscript.length == 0) <ci>{mname}</ci> else <ci><msub><mi>{mname}</mi><mrow>{subscript.map(a => <mn>{a}</mn>)}</mrow></msub></ci>) else (if (subscript.length == 0) <ci><msup><mi>{mname}</mi><mrow>{for (i <- 0 until prime) yield <mo>I</mo>}</mrow></msup></ci> else <ci><msubsup><mi>{mname}</mi><mrow>{subscript.map(a => <mn>{a}</mn>)}</mrow><mrow>{for (i <- 0 until prime) yield <mo>I</mo>}</mrow></msubsup></ci>)
  def mname = greek.getOrElse(name, name)
  val greek = Map(
    "Alpha"   -> "\u0391",
    "Beta"    -> "\u0392",
    "Gamma"   -> "\u0393",
    "Delta"   -> "\u0394",
    "Epsilon" -> "\u0395",
    "Zeta"    -> "\u0396",
    "Eta"     -> "\u0397",
    "Theta"   -> "\u0398",
    "Iota"    -> "\u0399",
    "Kappa"   -> "\u039A",
    "Lambda"  -> "\u039B",
    "Mu"      -> "\u039C",
    "Nu"      -> "\u039D",
    "Xi"      -> "\u039E",
    "Pi"      -> "\u03A0",
    "Rho"     -> "\u03A1",
    "Sigma"   -> "\u03A3",
    "Tau"     -> "\u03A4",
    "Upsilon" -> "\u03A5",
    "Phi"     -> "\u03A6",
    "Chi"     -> "\u03A7",
    "Psi"     -> "\u03A8",
    "Omega"   -> "\u03A9",
    "alpha"   -> "\u03B1",
    "beta"    -> "\u03B2",
    "gamma"   -> "\u03B3",
    "delta"   -> "\u03B4",
    "epsilon" -> "\u03B5",
    "zeta"    -> "\u03B6",
    "eta"     -> "\u03B7",
    "theta"   -> "\u03B8",
    "iota"    -> "\u03B9",
    "kappa"   -> "\u03BA",
    "lambda"  -> "\u03BB",
    "mu"      -> "\u03BC",
    "nu"      -> "\u03BD",
    "xi"      -> "\u03BE",
    "pi"      -> "\u03C0",
    "rho"     -> "\u03C1",
    "sigma"   -> "\u03C3",
    "tau"     -> "\u03C4",
    "upsilon" -> "\u03C5",
    "phi"     -> "\u03C6",
    "chi"     -> "\u03C7",
    "psi"     -> "\u03C8",
    "omega"   -> "\u03C9"
  )
}

object Variable extends Ordering[Variable] {
  def compare(x: Variable, y: Variable) = {
    val c = String.compare(x.name, y.name)
    if (c < 0) -1
    else if (c > 0) 1
    else {
      val c = Int.compare(x.prime, y.prime)
      if (c < 0) -1
      else if (c > 0) 1
      else Iterable[Int].compare(x.subscript, y.subscript)
    }
  }
  implicit def string2variable(s: String): Variable = apply(s)
  implicit def symbol2variable(s: Symbol): Variable = apply(s.name)
  def apply(name: String, subscript: Int*) = new Variable(name, 0, subscript.toArray)
  def apply(name: String, prime: Int, subscript: Array[Int]) = new Variable(name, prime, subscript)
}
