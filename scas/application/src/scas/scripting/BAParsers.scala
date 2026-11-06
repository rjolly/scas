package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred
import scas.variable.Variable
import scas.factors.BooleanAlgebra
import scas.residue.BooleanAlgebra.Element
import BAParsers.Impl

class BAParsers(using BooleanAlgebra.Impl[Array[Int]]) extends Impl[Array[Int]] {
  def this(dummy: Boolean) = this(using BooleanAlgebra(0)())
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[Element[Array[Int]]] = ("mod") ~ ("(" ~> expr) ~ rep("," ~> expr) <~ ")" ^^ {
    case "mod" ~ expr ~ list => mod(expr, list*)
  }
  def generator: Parser[Element[Array[Int]]] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.ring.pp.variables
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      structure.extend(a)
      structure.generator(variables.length)
    }
  }
  def base: Parser[Element[Array[Int]]] = BooleanParsers().base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"
}

object BAParsers {
  trait Impl[M] extends BooleanRingParsers[Element[M]] {
    given structure: BooleanAlgebra.Impl[M] = deferred
    def mod(expr: Element[M], list: Element[M]*) = {
      structure.update(list.map(!_)*)
      !structure(!expr)
    }
  }
}
