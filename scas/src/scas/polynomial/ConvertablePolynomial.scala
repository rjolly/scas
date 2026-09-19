package scas.polynomial

import scala.compiletime.deferred
import scas.power.ArrayPowerProduct

trait ConvertablePolynomial[T, C, N] extends Polynomial[T, C, Array[N]] {
  given pp: ArrayPowerProduct[N] = deferred
  extension (x: T) def convert(from: ArrayPowerProduct[N]) = x.map((s, a) => (s.convert(from), a)).sort
}
