package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C, N](using val ring: UniqueFactorizationDomain[C], val pp: ArrayPowerProduct[N])(using ClassTag[N], Numeric[N]) extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGB[C, N] = this
  object Implicits {
    export PolynomialWithGB.this.{instance, coef2poly}
  }
  def newInstance(pp: ArrayPowerProduct[N]) = new PolynomialWithGB(using ring, pp)
}
