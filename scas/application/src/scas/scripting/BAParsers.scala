package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import scas.base.Boolean

type BA = Element[Boolean, Array[Int]]

class BAParsers(using var structure: BooleanAlgebra) extends BooleanRingParsers[BA] {
  def this(dummy: Boolean) = this(using BooleanAlgebra())
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
    if (variables.contains(a)) structure.generator(variables.indexOf(a))
    else {
      val s = variables ++ Seq(a)
      structure = BooleanAlgebra(s*)
      structure.generator(variables.length)
    }
  }
  def base: Parser[BA] = BooleanParsers.base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"

  def reset: Unit = {
    structure = BooleanAlgebra()
  }
}

object BAParsers extends BAParsers(false)
