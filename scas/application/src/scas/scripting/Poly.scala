package scas.scripting

import scas.power.growable.Lexicographic
import scas.polynomial.tree.growable.PolynomialWithGB
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

type Poly = Element[BigInteger, Array[Int]]

object Poly {
  def apply(variables: Variable*) = new scas.polynomial.tree.growable.PolynomialWithGB(using BigInteger, new Lexicographic[Int](variables*))
}
