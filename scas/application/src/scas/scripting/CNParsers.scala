package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scas.residue.growable.ComplexNumber
import scas.variable.{Variable, Function}
import scas.base.{Complex, BigInteger}
import BigInteger.given

class CNParsers(dummy: Boolean) extends FieldParsers[Complex] {
  override given structure: ComplexNumber = new ComplexNumber
  import structure.{real, imag, conjugate}
  def generator(a: Function[Complex]) = {
    val variables = structure.ring.pp.variables
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      structure.extend(a)
      val x = structure.generator(variables.length)
      structure.update(a.parameter(0) - x\2)
      x
    }
  }
  def number: Parser[Complex] = RationalParsers.base ^^ { structure.ring(_) }
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[Complex] = (literal("sqrt") | literal("real") | literal("imag") | literal("conjugate")) ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x => generator(Variable.sqrt(x))
    case "real" ~ x => real(x)
    case "imag" ~ x => imag(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = number | function | "(" ~> expr <~ ")"
}
