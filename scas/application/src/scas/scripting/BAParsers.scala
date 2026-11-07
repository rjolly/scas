package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scas.variable.Variable
import scas.factors.BooleanAlgebra
import scas.residue.BooleanAlgebra.Element

class BAParsers(using BooleanAlgebra.Impl[Array[Int]]) extends BooleanRingParsers[Element[Array[Int]]] {
  def this(dummy: Boolean) = this(using BooleanAlgebra(0)())
  override given structure: BooleanAlgebra.Impl[Array[Int]] = summon
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[Element[Array[Int]]] = ("mod") ~ ("(" ~> expr) ~ rep("," ~> expr) <~ ")" ^^ {
    case "mod" ~ expr ~ list => structure.mod(expr, list*)
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
