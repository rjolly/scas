package scas.application

import scas._
import Implicits.{CC, infixOps}
import Complex.{sqrt, realPart, imaginaryPart, conjugate}
import Parsers._

object ComplexParsers extends UFDParsers[Complex] {
  val structure = CC
  def function: Parser[Complex] = ("sqrt" | "real" | "imag" | "conjugate") ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x if (x >< BigInteger(-1)) => sqrt(x)
    case "real" ~ x => realPart(x)
    case "imag" ~ x => imaginaryPart(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = RationalParsers.base ^^ { Complex(_) } | function | "(" ~> expr <~ ")"
}
