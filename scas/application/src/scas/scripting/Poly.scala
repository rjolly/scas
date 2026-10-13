package scas.scripting

import scas.power.growable.Lexicographic
import scas.polynomial.tree.growable.PolynomialOverUFDWithGB
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

type Poly = Element[BigInteger, Array[Int]]

object Poly {
  def apply(variables: Variable*) = new PolynomialOverUFDWithGB(using BigInteger, new Lexicographic[Int](variables*))
}
