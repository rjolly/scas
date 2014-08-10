package scas.polynomial.array.ufd.gb

import scala.reflect.ClassTag
import scas.Variable
import scas.polynomial.ArrayPolynomial
import scas.polynomial.ufd.PolynomialOverUFD
import scas.power.offset.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithGB.Element

class PolynomialWithGB[C, N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]], val cm1: ClassTag[C], val cm2: ClassTag[N]) extends ArrayPolynomial[Element[C, N], C, N] with scas.polynomial.ufd.gb.PolynomialWithGB[Element[C, N], C, N] {
  val self = this
  def apply(value: (Array[C], Array[N])) = new Element(value)(this)
}

object PolynomialWithGB {
  def apply[C](ring: UniqueFactorizationDomain[C], variables: Variable*)(implicit cm: ClassTag[Element[C, Int]], cm1: ClassTag[C]): PolynomialWithGB[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit cm: ClassTag[Element[C, N]], cm1: ClassTag[C], cm2: ClassTag[N]) = new PolynomialWithGB(ring, pp)

  class Element[C, N](val value: (Array[C], Array[N]))(val factory: PolynomialWithGB[C, N]) extends ArrayPolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomialWithGB[D, C, N](value: D)(implicit f: D => C, factory: PolynomialWithGB[C, N]) = factory(value)
  }
}
