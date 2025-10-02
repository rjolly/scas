package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable

type BA = Element[Boolean, Array[Int]]

class BAParsers(using_structure: BooleanAlgebra)(b: BooleanParsers) extends BooleanRingParsers[BA] {
  def this(b: BooleanParsers) = this(BooleanAlgebra())(b)
  override given structure: BooleanAlgebra = using_structure
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[BA] = ("mod") ~ ("(" ~> expr) ~ rep("," ~> expr) <~ ")" ^^ {
    case "mod" ~ expr ~ list => mod(expr.convert, list.map(_.convert)*)
  }
  def mod(expr: BA, list: BA*) = {
    structure.update(list.map(!_)*)
    !structure(!expr)
  }
  def generator: Parser[BA] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.ring.pp.variables
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      structure.extend(a)
      structure.generator(variables.length)
    }
  }
  def base: Parser[BA] = b.base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"
}
