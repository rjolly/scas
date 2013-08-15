package scas.application

import scas._
import Implicits.{ZZ, infixUFDOps}
import Parsers._

object RF extends UFDParsers[RF] {
  implicit def structure = r

  def updated = RationalFunction.integral(Poly.r)

  var r = updated

  def convert(x: RF) = if (x.factory == r) x else r.convert(x)

  def reset = {
    Poly.reset
    r = updated
  }

  def base: Parser[RF] = Poly.base ^^ {
    case x if (x.factory == r.ring) => r(x)
    case x => {
      r = updated
      r(x)
    }
  } | "(" ~> expr <~ ")"
  override def unsignedTerm: Parser[RF] = unsignedFactor ~ ((("*" | "/") ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => convert(x) * convert(y)
      case (x, "/" ~ y) => convert(x) / convert(y)
    }
  }
  override def expr: Parser[RF] = term ~ ((("+" | "-") ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => convert(x) + convert(y)
      case (x, "-" ~ y) => convert(x) - convert(y)
    }
  }
  override def obj: Parser[MathObject] = expr
  def graph: Parser[Graph] = "graph" ~> ("(" ~> expr) ~ ("," ~> Var.parser) <~ ")" ^^ {
    case expr ~ variable => Graph(expr.function(variable))
  }
}
