package scas.scripting

import Parsers._
import scas.polynomial.ufd.PolynomialOverUFD
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

class PolyParsers(using var structure: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) extends UFDParsers[Poly] {
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

  def reset: Unit = {
    structure = Poly()
  }
}

object PolyParsers extends PolyParsers(false)
