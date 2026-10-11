package scas.polynomial.tree

import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element
import MultivariatePolynomial.Impl

abstract class MultivariatePolynomial[C](using UniqueFactorizationDomain[C], ArrayPowerProduct[Int]) extends Impl[C, Int] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  def this(ring: UniqueFactorizationDomain[C])(variables: Variable*) = this(using ring, new Lexicographic[Int](variables*))
  given instance: MultivariatePolynomial[C] = this
}

object MultivariatePolynomial {
  trait Impl[C, N] extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, N]

  def withSimpleGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSimpleGCD(ring)(s.map(~_)*)
  def withPrimitiveGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithPrimitiveGCD(ring)(s.map(~_)*)
  def withSubresGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSubresGCD(ring)(s.map(~_)*)
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new MultivariatePolynomialOverField(ring)(s.map(~_)*)
}
