package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.power.compact.BinaryPowerProduct

trait BinaryPolynomial[T, C] extends ConvertablePolynomial[T, C, Int] {
  given pp: BinaryPowerProduct = deferred
}
