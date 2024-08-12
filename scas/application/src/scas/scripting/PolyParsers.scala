package scas.scripting

import Parsers._
import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.PolynomialOverUFD
import PolyParsers.newInstance
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

type Poly = Element[BigInteger, Array[Int]]

class PolyParsers(using var structure: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) extends RingParsers[Poly] {
  def function: Parser[Poly] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x if (x <> 0) => factor(x)
  }
  def factor(x: BigInteger) = {
    val map = Int.factor(BigInteger.abs(x))
    map.foldLeft(structure.fromInt(BigInteger.signum(x))) { case (l, (a, b)) =>
      val x = generator(Variable(a.toString))
      l.convert * x \ b
    }
  }
  def generator: Parser[Poly] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.variables
    if (variables.contains(a)) structure.generator(variables.indexOf(a))
    else {
      val s = variables ++ Seq(a)
      structure = newInstance(s*)
      structure.generator(variables.length)
    }
  }
  def base: Parser[Poly] = Int.base ^^ { structure(_) } | function | generator | "(" ~> expr <~ ")"
  override def unsignedTerm: Parser[Poly] = unsignedFactor ~ rep("*" ~ factor) ^^ {
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
}

object PolyParsers {
  def apply(variables: Variable*) = new PolyParsers(using newInstance(variables*))
  def newInstance(variables: Variable*) = PolynomialWithSubresGCD(variables*)
}
