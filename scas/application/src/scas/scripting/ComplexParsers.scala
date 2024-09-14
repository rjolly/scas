package scas.scripting

import Parsers._
import scas.base.{BigInteger, Rational, Complex}
import BigInteger.given
import Rational.Implicits.given
import Complex.{sqrt, real, imag, conjugate, coef2poly, ring}

object ComplexParsers extends FieldParsers[Complex] {
  given structure: Complex.Impl = Complex
  def number: Parser[Complex] = RationalParsers.base ^^ { ring(_) }
  def function: Parser[Complex] = (literal("sqrt") | literal("real") | literal("imag") | literal("conjugate")) ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x if (x >< -1) => sqrt(x)
    case "real" ~ x => real(x)
    case "imag" ~ x => imag(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = number | function | "(" ~> expr <~ ")"
}
