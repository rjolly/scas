package scas.polynomial.tree

import scala.reflect.ClassTag
import scala.collection.SortedMap
import scas.Variable
import scas.polynomial.TreePolynomial
import scas.power.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithSugar.Element

trait PolynomialWithSugar[C, N] extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.PolynomialWithSugar[Element[C, N], C, N] {
  def apply(x: Element[C, N], sugar: Long) = apply(x.value, sugar)
  def apply(value: SortedMap[Array[N], C], sugar: Long) = new Element(value, sugar)(this)
  def apply(value: SortedMap[Array[N], C]) = apply(value, 0l)
}

object PolynomialWithSugar {
  def apply[C](ring: UniqueFactorizationDomain[C], variables: Variable*): PolynomialWithSugar[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSloppy(ring, pp)
  def fussy[C](ring: UniqueFactorizationDomain[C], variables: Variable*): PolynomialWithSugar[C, Int] = fussy(ring, PowerProduct(variables: _*))
  def fussy[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithFussy(ring, pp)

  class Element[C, N](val value: SortedMap[Array[N], C], val sugar: Long)(val factory: PolynomialWithSugar[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.PolynomialWithSugar.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomialWithSugar[D, C, N](value: D)(implicit f: D => C, factory: PolynomialWithSugar[C, N]) = factory(value)
  }
}
