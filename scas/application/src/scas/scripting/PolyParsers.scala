package scas.scripting

import Parsers.*
import scas.polynomial.ufd.PolynomialOverUFD
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

class PolyParsers(var using_structure: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) extends UFDParsers[Poly] {
  def this(dummy: Boolean, variables: Variable*) = this(Poly(variables*))
  override given structure: () => PolynomialOverUFD[Poly, BigInteger, Array[Int]] = using_structure
  def generator: Parser[Poly] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.pp.variables
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      val s = variables ++ Seq(a)
      using_structure = Poly(s*)
      structure.generator(variables.length)
    }
  }
  def base: Parser[Poly] = Int.base ^^ { structure(_) } | generator | "(" ~> expr <~ ")"

  def reset: Unit = {
    using_structure = Poly()
  }
}

object PolyParsers extends PolyParsers(false)
