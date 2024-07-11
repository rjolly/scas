package scas.polynomial.tree

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import scala.compiletime.deferred
import scas.variable.Variable
import Variable.string2variable
import TreePolynomial.Element

trait MultivariatePolynomial[C, M] extends TreePolynomial[C, M] with scas.polynomial.MultivariatePolynomial[Element, C, M] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, M]] {
  given instance: MultivariatePolynomial[C, M] = deferred
}

object MultivariatePolynomial {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C, Array[Int]] = new PolynomialWithSimpleGCD(using ring)(s.map(string2variable)*)
  def withPrimitiveGCD[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C, Array[Int]] = new PolynomialWithPrimitiveGCD(using ring)(s.map(string2variable)*)
  def withSubresGCD[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C, Array[Int]] = new PolynomialWithSubresGCD(using ring)(s.map(string2variable)*)
}
