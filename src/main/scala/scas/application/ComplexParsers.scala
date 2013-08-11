package scas.application

import scas._
import Implicits.{CC, infixOps}
import Complex.{sqrt, real, imag, conjugate}
import Parsers._

object ComplexParsers extends UFDParsers[Complex] {
  val structure = CC
  def function: Parser[Complex] = ("sqrt" | "real" | "imag" | "conjugate") ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x if (x >< -1) => sqrt(x)
    case "real" ~ x => real(x)
    case "imag" ~ x => imag(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = Double.base ^^ { Complex(_) } | function | "(" ~> expr <~ ")"
}
