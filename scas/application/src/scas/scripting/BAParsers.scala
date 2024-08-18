package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.polynomial.TreePolynomial.Element
import scas.residue.BooleanAlgebra
import scas.variable.Variable
import BAParsers.newInstance

type BA = Element[Boolean, Array[Int]]

class BAParsers(using var structure: BooleanAlgebra) extends BooleanRingParsers[BA] {
  def generator: Parser[BA] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.variables
    if (variables.contains(a)) structure.generator(variables.indexOf(a))
    else {
      val s = variables ++ Seq(a)
      structure = newInstance(s*)
      structure.generator(variables.length)
    }
  }
  def base: Parser[BA] = BooleanParsers.base ^^ { structure(_) } | generator | "(" ~> expr <~ ")"
  @nowarn("msg=match may not be exhaustive")
  override def function: Parser[BA] = base ~ "=>" ~ base ^^ {
    case x ~ "=>" ~ y => x.convert >> y.convert
  }
  override def conj: Parser[BA] = term ~ rep("&" ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "&" ~ y) => x.convert && y.convert
    }
  }
  override def disj: Parser[BA] = conj ~ rep("^" ~ conj) ^^ {
    case conj ~ list => list.foldLeft(conj) {
      case (x, "^" ~ y) => x.convert ^ y.convert
    }
  }
  override def expr: Parser[BA] = disj ~ rep("|" ~ disj) ^^ {
    case disj ~ list => list.foldLeft(disj) {
      case (x, "|" ~ y) => x.convert || y.convert
    }
  }
}

object BAParsers {
  def apply(variables: Variable*) = new BAParsers(using newInstance(variables*))
  def newInstance(variables: Variable*) = BooleanAlgebra(variables*)
}
