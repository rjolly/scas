package scas.quotient

import scas.structure.commutative.Field
import scas.structure.commutative.Quotient.Element
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.PolynomialOverField
import scas.variable.Variable

class RationalFunctionOverField[C](using val ring: PolynomialOverField[UnivariatePolynomial.Element[C], C, Array[Int]]) extends Quotient[UnivariatePolynomial.Element[C], C, Array[Int]] {
  def this(ring: Field[C])(s: Variable) = this(using new UnivariatePolynomial(using ring)(s))
  override def apply(x: Element[UnivariatePolynomial.Element[C]]) = {
    val Element(n, d) = x
    val c = ring.gcd(n, d)
    val gcd = c%/ ring.lastCoefficient(c)%* ring.lastCoefficient(d)
    Element(n / gcd, d / gcd)
  }
}
