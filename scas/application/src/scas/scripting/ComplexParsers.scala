package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scas.base.{BigInteger, Rational, Complex}
import BigInteger.given
import Rational.given
import Complex.{sqrt, real, imag, conjugate, coef2poly, ring}

object ComplexParsers extends FieldParsers[Complex] {
  override given structure: () => Complex.Impl = Complex
  def number: Parser[Complex] = RationalParsers.base ^^ { ring(_) }
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[Complex] = (literal("sqrt") | literal("real") | literal("imag") | literal("conjugate")) ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x if (x >< -1) => sqrt(x)
    case "real" ~ x => real(x)
    case "imag" ~ x => imag(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = number | function | "(" ~> expr <~ ")"
}
