package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scas.residue.growable.ComplexNumber
import scas.base.Complex

class CNParsers(dummy: Boolean) extends FieldParsers[Complex] {
  override given structure: ComplexNumber = new ComplexNumber
  import structure.{sqrt, real, imag, conjugate}
  def number: Parser[Complex] = RationalParsers.base ^^ { structure.ring(_) }
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[Complex] = (literal("sqrt") | literal("real") | literal("imag") | literal("conjugate")) ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x => sqrt(x)
    case "real" ~ x => real(x)
    case "imag" ~ x => imag(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = number | function | "(" ~> expr <~ ")"
}
