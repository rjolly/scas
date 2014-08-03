package scas.application.polynomial

import scala.reflect.ClassTag
import scas.polynomial.GrowablePolynomial
import scas.polynomial.tree.ufd.gb.PolynomialWithSugar
import scas.power.growable.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithSugar.Element

class Polynomial[C, N](val ring: UniqueFactorizationDomain[C], var pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends PolynomialWithSugar[C, N] with GrowablePolynomial[Element[C, N], C, N]

object Polynomial {
  def apply[C](ring: UniqueFactorizationDomain[C]) = new Polynomial(ring, PowerProduct())
}
