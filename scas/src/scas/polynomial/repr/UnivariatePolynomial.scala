package scas.polynomial.repr

import scala.reflect.ClassTag
import scas.polynomial.PolynomialWithRepr
import scas.polynomial.ufd.PolynomialOverField
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C, M](using scas.polynomial.ufd.UnivariatePolynomial[T, C, M])(dimension: Int) extends PolynomialWithRepr[T, C, M](dimension) with scas.polynomial.ufd.UnivariatePolynomial[Element[T], C, M] {
  override given factory: PolynomialOverField[T, C, M] = summon[PolynomialOverField[T, C, M]]
  override given ring: Field[C] = factory.ring
  override given pp: PowerProduct[M] = factory.pp
}
