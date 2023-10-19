package scas.polynomial.tree

import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.polynomial.impl.MultivariatePolynomial
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.splittable.Lexicographic
import scas.util.unary_~

class PolynomialWithSimpleGCD[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with MultivariatePolynomial[Element, C, M] with scas.polynomial.impl.PolynomialWithSimpleGCD[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: PolynomialWithSimpleGCD[C, M] = this
  def newInstance = [C] => (ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) => new PolynomialWithSimpleGCD(using ring, pp)
}

object PolynomialWithSimpleGCD {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithSimpleGCD(using ring, new Lexicographic[Int](s.map(~_): _*))
}
