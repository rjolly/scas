package scas.polynomial.tree

import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.splittable.Lexicographic
import scas.util.unary_~

class PolynomialWithPrimitiveGCD[C, M](using ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends TreePolynomial[C, M] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, M] with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element[C, M], C, M] with PolynomialOverUFD[Element[C, M], C, M] {
  given instance: PolynomialWithPrimitiveGCD[C, M] = this
  def split = new PolynomialWithPrimitiveGCD(using new PolynomialWithPrimitiveGCD(using ring, take), drop)
}

object PolynomialWithPrimitiveGCD {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithPrimitiveGCD(using ring, new Lexicographic[Int](s.map(~_): _*))
}
