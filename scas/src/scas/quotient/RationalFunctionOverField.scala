package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.{MultivariatePolynomial, MultivariatePolynomialOverField}
import scas.polynomial.PolynomialOverField
import scas.variable.Variable

class RationalFunctionOverField[C](using val ring: PolynomialOverField[MultivariatePolynomial.Element[C], C, Array[Int]]) extends Quotient[MultivariatePolynomial.Element[C], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable*) = this(using new MultivariatePolynomialOverField(using ring)(s*))
  override def apply(x: Element[MultivariatePolynomial.Element[C]]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ ring.lastCoefficient(c)%* ring.lastCoefficient(d)
    Element(n / gcd, d / gcd)
  }
}
