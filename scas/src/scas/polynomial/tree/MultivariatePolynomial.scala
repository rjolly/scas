package scas.polynomial.tree

import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.splittable.Lexicographic
import scas.util.unary_~

abstract class MultivariatePolynomial[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.impl.MultivariatePolynomial[Element, C, M]

object MultivariatePolynomial {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithSimpleGCD(using ring, new Lexicographic[Int](s.map(~_): _*))
  def withPrimitiveGCD[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithPrimitiveGCD(using ring, new Lexicographic[Int](s.map(~_): _*))
  def withSubresGCD[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithSubresGCD(using ring, new Lexicographic[Int](s.map(~_): _*))
}
