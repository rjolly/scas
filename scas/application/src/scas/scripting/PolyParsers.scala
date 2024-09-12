package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.polynomial.PolynomialOverUFD
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

class PolyParsers(using var structure: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) extends RingParsers[Poly] {
  def this(dummy: Boolean, variables: Variable*) = this(using Poly(variables*))
  def generator: Parser[Poly] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.variables
    if (variables.contains(a)) structure.generator(variables.indexOf(a))
    else {
      val s = variables ++ Seq(a)
      structure = Poly(s*)
      structure.generator(variables.length)
    }
  }
  def base: Parser[Poly] = Int.base ^^ { structure(_) } | generator | "(" ~> expr <~ ")"
  override def unsignedTerm: Parser[Poly] = unsignedFactor ~ rep((literal("*") | literal("/")) ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
      case (x, "/" ~ y) => x.convert / y.convert
    }
  }
  override def expr: Parser[Poly] = term ~ rep((literal("+") | literal("-")) ~ unsignedTerm) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "+" ~ y) => x.convert + y.convert
      case (x, "-" ~ y) => x.convert - y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = expr ~ (literal("=") | literal("<>")) ~ expr ^^ {
    case x ~ "=" ~ y => x.convert >< y.convert
    case x ~ "<>" ~ y => x.convert <> y.convert
  }

  def reset: Unit = {
    structure = Poly()
  }
}

object PolyParsers extends PolyParsers(false)
