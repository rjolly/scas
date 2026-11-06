package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred
import scas.variable.Variable
import scas.factors.BooleanAlgebra
import scas.residue.BooleanAlgebra.Element

class BAParsers(using BooleanAlgebra) extends BAParsers.Impl {
  def this(dummy: Boolean) = this(using BooleanAlgebra())
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[Element] = ("mod") ~ ("(" ~> expr) ~ rep("," ~> expr) <~ ")" ^^ {
    case "mod" ~ expr ~ list => mod(expr, list*)
  }
  def generator: Parser[Element] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.ring.pp.variables
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      structure.extend(a)
      structure.generator(variables.length)
    }
  }
  def base: Parser[Element] = BooleanParsers().base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"
}

object BAParsers {
  trait Impl extends BooleanRingParsers[Element] {
    given structure: BooleanAlgebra = deferred
    def mod(expr: Element, list: Element*) = {
      structure.update(list.map(!_)*)
      !structure(!expr)
    }
  }
}
