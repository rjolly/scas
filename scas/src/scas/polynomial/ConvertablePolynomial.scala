package scas.polynomial

import scala.compiletime.deferred
import scas.power.splitable.ArrayPowerProduct

trait ConvertablePolynomial[T, C, N] extends Polynomial[T, C, Array[N]] {
  given pp: ArrayPowerProduct[N] = deferred
  def newInstance(pp: ArrayPowerProduct[N]): ConvertablePolynomial[T, C, N]
  extension (x: T) def convert(from: ArrayPowerProduct[N]) = x.map((s, a) => (s.convert(from), a)).sort
}
