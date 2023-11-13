package scas.variable

import scas.prettyprint.Show

abstract class Variable {
  def toMathML: String
  override def hashCode = toString.hashCode
  override def equals(that: Any) = toString.equals(that.toString)
  extension (name: String) def toMathML = Map(
    "Alpha"   -> "&#x00391;",
    "Beta"    -> "&#x00392;",
    "Gamma"   -> "&#x00393;",
    "Delta"   -> "&#x00394;",
    "Epsilon" -> "&#x00395;",
    "Zeta"    -> "&#x00396;",
    "Eta"     -> "&#x00397;",
    "Theta"   -> "&#x00398;",
    "Iota"    -> "&#x00399;",
    "Kappa"   -> "&#x0039A;",
    "Lambda"  -> "&#x0039B;",
    "Mu"      -> "&#x0039C;",
    "Nu"      -> "&#x0039D;",
    "Xi"      -> "&#x0039E;",
    "Pi"      -> "&#x003A0;",
    "Rho"     -> "&#x003A1;",
    "Sigma"   -> "&#x003A3;",
    "Tau"     -> "&#x003A4;",
    "Upsilon" -> "&#x003A5;",
    "Phi"     -> "&#x003A6;",
    "Chi"     -> "&#x003A7;",
    "Psi"     -> "&#x003A8;",
    "Omega"   -> "&#x003A9;",
    "alpha"   -> "&#x003B1;",
    "beta"    -> "&#x003B2;",
    "gamma"   -> "&#x003B3;",
    "delta"   -> "&#x003B4;",
    "epsilon" -> "&#x003B5;",
    "zeta"    -> "&#x003B6;",
    "eta"     -> "&#x003B7;",
    "theta"   -> "&#x003B8;",
    "iota"    -> "&#x003B9;",
    "kappa"   -> "&#x003BA;",
    "lambda"  -> "&#x003BB;",
    "mu"      -> "&#x003BC;",
    "nu"      -> "&#x003BD;",
    "xi"      -> "&#x003BE;",
    "pi"      -> "&#x003C0;",
    "rho"     -> "&#x003C1;",
    "sigma"   -> "&#x003C3;",
    "tau"     -> "&#x003C4;",
    "upsilon" -> "&#x003C5;",
    "phi"     -> "&#x003C6;",
    "chi"     -> "&#x003C7;",
    "psi"     -> "&#x003C8;",
    "omega"   -> "&#x003C9"
  ).getOrElse(name, name)
}

object Variable {
  given string2variable: (String => Variable) = apply(_)
  given Show[Variable] with {
    extension (x: Variable) {
      def toCode(level: Level) = x.toString
      def toMathML = x.toMathML
    }
  }

  def apply(name: String, subscript: Int*): Variable = new Constant(name, 0, subscript: _*)
  def apply(name: String, prime: Int, subscript: Int*): Variable = new Constant(name, prime, subscript: _*)

  def function[T : Show](name: String, parameter: T*): Variable = new Function(name, parameter: _*)
  def sqrt[T : Show](x: T): Variable = new Sqrt(x)
}
