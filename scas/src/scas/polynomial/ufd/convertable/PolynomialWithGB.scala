package scas.polynomial.ufd.convertable

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.structure.commutative.UniqueFactorizationDomain
import scas.power.ArrayPowerProduct

trait PolynomialWithGB[T[N], C, N] extends scas.polynomial.ufd.PolynomialWithGB[T, C, Array[N]] with ConvertablePolynomial[T[Array[N]], C, N] {
  def newInstance(pp: ArrayPowerProduct[N]): PolynomialWithGB[T, C, N]

  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T[Array[N]]*): PolynomialWithGB[T, C, N] = {
    same(s*)
    this
  }
}
