package scas.polynomial.ufd.convertable

import scas.power.splitable.ArrayPowerProduct

trait PolynomialOverFieldWithGB[T[N], C, N] extends PolynomialWithGB[T, C, N] with PolynomialWithModInverse[T[Array[N]], C, N] with scas.polynomial.ufd.PolynomialOverFieldWithGB[T, C, Array[N]] {
  override def newInstance(pp: ArrayPowerProduct[N]): PolynomialOverFieldWithGB[T, C, N]
}
