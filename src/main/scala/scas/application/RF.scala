package scas.application

import scas._
import Implicits.{ZZ, infixUFDOps, infixPowerProductOps}
import Parsers._

object RF extends UFDParsers[Element] {
  implicit def structure = r

  def ring(variables: Variable*) = RationalFunction.integral(MultivariatePolynomial(ZZ, PowerProduct(variables: _*)))

  var r = ring()

  def convert(x: Element) = if (x.factory == r) x else r.convert(x)

  def variable: Parser[Variable] = name ~ prime ~ (subscript*) ^^ {
    case name ~ prime ~ list => Variable(name, prime, list.toArray)
  }

  def reset = {
    r = ring()
  }

  def function: Parser[Element] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x if (x <> 0) => factor(x)
  }
  def factor(x: BigInteger) = {
    val map = BigInteger.factor(BigInteger.abs(x))
    val s = map.keys.toArray.map({ x: BigInteger => Variable(x.toString) })
    val variables = r.variables.union(s).distinct
    if (variables.length > r.variables.length) r = ring(variables: _*)
    implicit val p = r.ring.pp
    val m = (p.one /: map) { case (l, (a, b)) =>
      val x = p.generator(Variable(a.toString))
      l * pow(x, b)
    }
    r(r.ring(m, BigInteger.signum(x)))
  }
  def generator: Parser[Element] = variable ^^ { s: Variable => generator(s) }
  def generator(s: Variable) = {
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = ring(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def base: Parser[Element] = Int.base ^^ { r(_) } | function | generator | "(" ~> expr <~ ")"
  override def unsignedFactor: Parser[Element] = base ~ ((("**" | "^") ~> Int.factor)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(convert(x), exp)
      case None => convert(x)
    }
  }
  override def unsignedTerm: Parser[Element] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => convert(x) * convert(y)
      case (x, "/" ~ y) => convert(x) / convert(y)
    }
  }
  override def expr: Parser[Element] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => convert(x) + convert(y)
      case (x, "-" ~ y) => convert(x) - convert(y)
    }
  }
}
