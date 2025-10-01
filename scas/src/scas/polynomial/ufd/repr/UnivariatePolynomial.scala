package scas.polynomial.ufd.repr

import scala.reflect.ClassTag
import scas.polynomial.PolynomialWithRepr
import scas.structure.commutative.Field
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class UnivariatePolynomial[T : ClassTag, C, M](using scas.polynomial.ufd.UnivariatePolynomial[T, C, M])(val dimension: Int) extends PolynomialWithRepr[T, C, M] with scas.polynomial.ufd.UnivariatePolynomial[Element[T], C, M] {
  override given factory: scas.polynomial.ufd.UnivariatePolynomial[T, C, M] = summon
  override given ring: Field[C] = factory.ring
  override given pp: PowerProduct[M] = factory.pp
}
