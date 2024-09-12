package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.polynomial.TreePolynomial.Element
import scas.residue.BooleanAlgebra
import scas.variable.Variable

type BA = Element[Boolean, Array[Int]]

class BAParsers(using var structure: BooleanAlgebra) extends BooleanRingParsers[BA] {
  def this(dummy: Boolean) = this(using BooleanAlgebra())
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[BA] = ("mod") ~ ("(" ~> expr) ~ rep("," ~> expr) <~ ")" ^^ {
    case "mod" ~ expr ~ list => {
      structure.update(list.map(_.convert)*)
      !structure(expr.convert)
    }
  }
  def generator: Parser[BA] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.variables
    if (variables.contains(a)) structure.generator(variables.indexOf(a))
    else {
      val s = variables ++ Seq(a)
      structure = BooleanAlgebra(s*)
      structure.generator(variables.length)
    }
  }
  def base: Parser[BA] = BooleanParsers.base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"
  override def impl: Parser[BA] = term ~ rep("=>" ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "=>" ~ y) => x.convert >> y.convert
    }
  }
  override def conj: Parser[BA] = impl ~ rep("&" ~ impl) ^^ {
    case impl ~ list => list.foldLeft(impl) {
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
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x.convert >< y.convert
    case x ~ "<>" ~ y => x.convert <> y.convert
  }

  def reset: Unit = {
    structure = BooleanAlgebra()
  }
}

object BAParsers extends BAParsers(false)
