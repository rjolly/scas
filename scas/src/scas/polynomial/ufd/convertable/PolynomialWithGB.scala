package scas.polynomial.ufd.convertable

import scas.power.splitable.ArrayPowerProduct

trait PolynomialWithGB[T[N], C, N] extends PolynomialOverUFD[T[Array[N]], C, N] with scas.polynomial.ufd.PolynomialWithGB[T, C, Array[N]] {
  override def newInstance(pp: ArrayPowerProduct[N]): PolynomialWithGB[T, C, N]
}
