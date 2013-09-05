package scas.polynomial
package array

import scala.reflect.ClassTag
import scas.Variable
import scas.power.offset.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithSugar.Element

trait PolynomialWithSugar[C, N] extends ArrayPolynomial[Element[C, N], C, N] with scas.polynomial.PolynomialWithSugar[Element[C, N], C, N] {
  def apply(x: Element[C, N], sugar: Long) = apply(x.value, sugar)
  def apply(value: (Array[C], Array[N]), sugar: Long) = new Element(value, sugar)(this)
  def apply(value: (Array[C], Array[N])) = apply(value, 0l)
}

object PolynomialWithSugar {
  def apply[C](ring: UniqueFactorizationDomain[C], variables: Variable*)(implicit cm: ClassTag[Element[C, Int]], cm1: ClassTag[C]): PolynomialWithSugar[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit cm: ClassTag[Element[C, N]], cm1: ClassTag[C], cm2: ClassTag[N]) = new PolynomialWithSloppy(ring, pp)
  def fussy[C](ring: UniqueFactorizationDomain[C], variables: Variable*)(implicit cm: ClassTag[Element[C, Int]], cm1: ClassTag[C]): PolynomialWithSugar[C, Int] = fussy(ring, PowerProduct(variables: _*))
  def fussy[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit cm: ClassTag[Element[C, N]], cm1: ClassTag[C], cm2: ClassTag[N]) = new PolynomialWithFussy(ring, pp)

  class Element[C, N](val value: (Array[C], Array[N]), val sugar: Long)(val factory: PolynomialWithSugar[C, N]) extends ArrayPolynomial.Element[Element[C, N], C, N] with scas.polynomial.PolynomialWithSugar.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomialWithSugar[D, C, N](value: D)(implicit f: D => C, factory: PolynomialWithSugar[C, N]) = factory(value)
  }
}
