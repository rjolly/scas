package scas.polynomial.tree

import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element
import MultivariatePolynomial.Impl

abstract class MultivariatePolynomial[C : UniqueFactorizationDomain](variables: Variable*) extends Impl[C] {
  override given pp: ArrayPowerProduct[Int] = new Lexicographic(variables*)
}

object MultivariatePolynomial {
  trait Impl[C] extends TreePolynomial[C, Array[Int]] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, Int] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
    given instance: Impl[C] = this
  }

  def withSimpleGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSimpleGCD(using ring)(s.map(~_)*)
  def withPrimitiveGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithPrimitiveGCD(using ring)(s.map(~_)*)
  def withSubresGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSubresGCD(using ring)(s.map(~_)*)
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new MultivariatePolynomialOverField(using ring)(s.map(~_)*)
}
