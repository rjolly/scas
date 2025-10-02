package scas.scripting

import Parsers.*
import scas.polynomial.ufd.growable.PolynomialOverUFD
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

class PolyParsers(using PolynomialOverUFD[Poly, BigInteger, Array[Int]]) extends UFDParsers[Poly] {
  def this(dummy: Boolean, variables: Variable*) = this(using Poly(variables*))
  override given structure: PolynomialOverUFD[Poly, BigInteger, Array[Int]] = summon
  def generator: Parser[Poly] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    val variables = structure.pp.variables
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      structure.extend(a)
      structure.generator(variables.length)
    }
  }
  def base: Parser[Poly] = Int.base ^^ { structure(_) } | generator | "(" ~> expr <~ ")"
}
