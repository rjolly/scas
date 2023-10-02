package scas.polynomial.tree

import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.splittable.Lexicographic
import scas.util.unary_~

class PolynomialWithSubresGCD[C, M](using ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends TreePolynomial[C, M] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, M] with scas.polynomial.ufd.PolynomialWithSubresGCD[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: PolynomialWithSubresGCD[C, M] = this
  def split = new PolynomialWithSubresGCD(using new PolynomialWithSubresGCD(using ring, take), drop)
}

object PolynomialWithSubresGCD {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithSubresGCD(using ring, new Lexicographic[Int](s.map(~_): _*))
}
