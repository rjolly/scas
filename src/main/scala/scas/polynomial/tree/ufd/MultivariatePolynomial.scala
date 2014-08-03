package scas.polynomial.tree.ufd

import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.TreePolynomial
import scas.polynomial.ufd.PolynomialOverUFD
import scas.power.splittable.PowerProduct
import scas.structure.{UniqueFactorizationDomain, Field}
import MultivariatePolynomial.Element

trait MultivariatePolynomial[C, N] extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object MultivariatePolynomial {
  def withSimpleGCD[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSimpleGCD(ring, pp)
  def withPrimitiveGCD[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithPrimitiveGCD(ring, pp)
  def apply[C](ring: UniqueFactorizationDomain[C], variables: Variable*): MultivariatePolynomial[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSubresGCD(ring, pp)
  def apply[C, N](ring: Field[C], pp: PowerProduct[N]) = new PolynomialWithMonicGCD(ring, pp)

  class Element[C, N](val value: SortedMap[Array[N], C])(val factory: MultivariatePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2multivariatePolynomial[D, C, N](value: D)(implicit f: D => C, factory: MultivariatePolynomial[C, N]) = factory(value)
  }
}
