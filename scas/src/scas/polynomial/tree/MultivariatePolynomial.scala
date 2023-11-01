package scas.polynomial.tree

import scas.power.splittable.{PowerProduct, Lexicographic}
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.polynomial.PolynomialOverUFD
import scas.polynomial.impl.TreePolynomial
import scas.polynomial.TreePolynomial.Element

abstract class MultivariatePolynomial[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.impl.MultivariatePolynomial[Element, C, M] with PolynomialOverUFD[Element[C, M], C, M]

object MultivariatePolynomial {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithSimpleGCD(using ring, Lexicographic[Int](s: _*))
  def withPrimitiveGCD[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithPrimitiveGCD(using ring, Lexicographic[Int](s: _*))
  def withSubresGCD[C](ring: UniqueFactorizationDomain[C])(s: String*) = new PolynomialWithSubresGCD(using ring, Lexicographic[Int](s: _*))
}
