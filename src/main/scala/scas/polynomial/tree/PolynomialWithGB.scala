package scas.polynomial
package tree

import scala.reflect.ClassTag
import scala.collection.SortedMap
import scas.Variable
import scas.power.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithGB.Element

class PolynomialWithGB[C, N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.PolynomialWithGB[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object PolynomialWithGB {
  def apply[C](ring: UniqueFactorizationDomain[C], variables: Variable*): PolynomialWithGB[C, Int] = apply(ring, PowerProduct(variables: _*))
  def apply[C, N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithGB(ring, pp)

  class Element[C, N](val value: SortedMap[Array[N], C])(val factory: PolynomialWithGB[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with scas.polynomial.PolynomialOverUFD.Element[Element[C, N], C, N]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2polynomialWithGB[D, C, N](value: D)(implicit f: D => C, factory: PolynomialWithGB[C, N]) = factory(value)
  }
}
