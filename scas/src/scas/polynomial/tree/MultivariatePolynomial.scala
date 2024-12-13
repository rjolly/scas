package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

trait MultivariatePolynomial[C] extends TreePolynomial[C, Array[Int]] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, Array[Int]] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  def s: Seq[Variable]
  given pp: PowerProduct[Array[Int]] = new Lexicographic[Int](s*)
  given instance: MultivariatePolynomial[C] = this
}

object MultivariatePolynomial {
  def withSimpleGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSimpleGCD(using ring)(s.map(~_)*)
  def withPrimitiveGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithPrimitiveGCD(using ring)(s.map(~_)*)
  def withSubresGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSubresGCD(using ring)(s.map(~_)*)
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new MultivariatePolynomialOverField(using ring)(s.map(~_)*)
}
