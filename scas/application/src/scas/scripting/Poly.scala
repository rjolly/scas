package scas.scripting

import scas.polynomial.tree.PolynomialWithSubresGCD
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

type Poly = Element[BigInteger, Array[Int]]

object Poly {
  def apply(variables: Variable*) = PolynomialWithSubresGCD(variables*)
}
