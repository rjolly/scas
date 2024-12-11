package scas.polynomial.repr

import scala.compiletime.deferred
import scas.polynomial.PolynomialWithRepr
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

trait PolynomialOverField[T, C, M] extends PolynomialWithRepr[T, C, M] with scas.polynomial.ufd.PolynomialOverField[Element[T], C, M] {
  given factory: scas.polynomial.ufd.PolynomialOverField[T, C, M] = deferred
  override given ring: Field[C] = factory.ring
  override given pp: PowerProduct[M] = factory.pp
}
