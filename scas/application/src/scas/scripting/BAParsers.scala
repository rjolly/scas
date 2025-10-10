package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable

type BA = Element[Boolean, Array[Int]]

class BAParsers(using BooleanAlgebra) extends BAParsers.Impl {
  def this(dummy: Boolean) = this(using BooleanAlgebra())
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[BA] = ("mod") ~ ("(" ~> expr) ~ rep("," ~> expr) <~ ")" ^^ {
    case "mod" ~ expr ~ list => mod(expr, list*)
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
  def base: Parser[BA] = BooleanParsers().base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"
}

object BAParsers {
  trait Impl extends BooleanRingParsers[BA] {
    given structure: BooleanAlgebra = deferred
    def mod(expr: BA, list: BA*) = {
      structure.update(list.map(!_)*)
      !structure(!expr)
    }
  }
}
