package scas.application

import scas._
import Implicits.{ZZ, infixUFDOps, infixPowerProductOps}
import Parsers._

object Poly extends RingParsers[Poly] {
  implicit def structure = r

  def updated(variables: Variable*) = MultivariatePolynomial(ZZ, PowerProduct(variables: _*))

  var r = updated()

  def convert(x: Poly) = if (x.factory == r) x else r.convert(x)

  def reset = {
    r = updated()
  }

  def function: Parser[Poly] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x if (x <> 0) => factor(x)
  }
  def factor(x: BigInteger) = {
    val map = BigInteger.factor(BigInteger.abs(x))
    val s = map.keys.toArray.map({ x: BigInteger => Variable(x.toString) })
    val variables = r.variables.union(s).distinct
    if (variables.length > r.variables.length) r = updated(variables: _*)
    implicit val p = r.pp
    val m = (p.one /: map) { case (l, (a, b)) =>
      val x = p.generator(Variable(a.toString))
      l * pow(x, b)
    }
    r(m, BigInteger.signum(x))
  }
  def generator: Parser[Poly] = Var.parser ^^ { s: Variable => generator(s) }
  def generator(s: Variable) = {
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = updated(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def base: Parser[Poly] = Int.base ^^ { r(_) } | function | generator | "(" ~> expr <~ ")"
  override def unsignedTerm: Parser[Poly] = unsignedFactor ~ (("*" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => convert(x) * convert(y)
      case (x, "/" ~ y) => convert(x) / convert(y)
    }
  }
  override def expr: Parser[Poly] = term ~ ((("+" | "-") ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => convert(x) + convert(y)
      case (x, "-" ~ y) => convert(x) - convert(y)
    }
  }
}
