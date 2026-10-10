package scas.polynomial.binary.growable

import scala.compiletime.deferred
import scas.polynomial.gb.GBEngine
import scas.power.compact.growable.BinaryPowerProduct
import scas.polynomial.GrowablePolynomial

trait Polynomial[T, C] extends scas.polynomial.binary.Polynomial[T, C] with GrowablePolynomial[T, C, Array[Int]] {
  given pp: BinaryPowerProduct = deferred
}
