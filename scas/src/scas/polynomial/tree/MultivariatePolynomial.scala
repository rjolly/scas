package scas.polynomial.tree

import scas.power.splittable.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

abstract class MultivariatePolynomial[C : UniqueFactorizationDomain, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.MultivariatePolynomial[Element, C, M] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, M]] {
  given instance: MultivariatePolynomial[C, M]
}

object MultivariatePolynomial {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C, Array[Int]] = new PolynomialWithSimpleGCD(using ring, Lexicographic[Int](s*))
  def withPrimitiveGCD[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C, Array[Int]] = new PolynomialWithPrimitiveGCD(using ring, Lexicographic[Int](s*))
  def withSubresGCD[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C, Array[Int]] = new PolynomialWithSubresGCD(using ring, Lexicographic[Int](s*))
}
