package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import scala.compiletime.deferred
import scas.variable.Variable
import Variable.string2variable
import TreePolynomial.Element

trait MultivariatePolynomial[C](s: Variable*) extends TreePolynomial[C, Array[Int]] with scas.polynomial.MultivariatePolynomial[Element, C, Array[Int]] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, Array[Int]]] {
  override given pp: PowerProduct[Array[Int]] = Lexicographic[Int](s*)
  given instance: MultivariatePolynomial[C] = deferred
}

object MultivariatePolynomial {
  def apply[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C] = new PolynomialWithSimpleGCD(using ring)(s.map(string2variable)*)
  def withPrimitiveGCD[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C] = new PolynomialWithPrimitiveGCD(using ring)(s.map(string2variable)*)
  def withSubresGCD[C](ring: UniqueFactorizationDomain[C])(s: String*): MultivariatePolynomial[C] = new PolynomialWithSubresGCD(using ring)(s.map(string2variable)*)
}
