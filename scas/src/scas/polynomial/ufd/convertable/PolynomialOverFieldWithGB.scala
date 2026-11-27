package scas.polynomial.ufd.convertable

import scas.structure.commutative.Field
import scas.power.splitable.ArrayPowerProduct

trait PolynomialOverFieldWithGB[T[N], C, N] extends PolynomialWithGB[T, C, N] with scas.polynomial.ufd.PolynomialOverFieldWithGB[T, C, Array[N]] {
  override def newInstance(pp: ArrayPowerProduct[N]): PolynomialOverFieldWithGB[T, C, N]

  extension (ring: Field[C]) override def apply(s: T[Array[N]]*): PolynomialOverFieldWithGB[T, C, N] = {
    same(s*)
    this
  }
}
