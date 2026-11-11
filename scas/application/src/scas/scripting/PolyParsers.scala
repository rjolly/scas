package scas.scripting

import Parsers.*
import scas.polynomial.growable.ufd.PolynomialOverUFD
import scas.variable.Variable
import scas.base.BigInteger

class PolyParsers(using PolynomialOverUFD[Poly, BigInteger, Array[Int]]) extends UFDParsers[Poly] {
  def this(dummy: Boolean, variables: Variable*) = this(using Poly(variables*))
  override given structure: PolynomialOverUFD[Poly, BigInteger, Array[Int]] = summon
  def variables = structure.pp.variables
  def generator: Parser[Poly] = Var.parser ^^ { generator(_) }
  def generator(a: Variable) = {
    if variables.contains(a) then structure.generator(variables.indexOf(a))
    else {
      structure.extend(a)
      structure.generator(variables.length - 1)
    }
  }
  def base: Parser[Poly] = Int.base ^^ { structure(_) } | generator | "(" ~> expr <~ ")"
}
