package scas.polynomial

import scala.compiletime.deferred
import scas.power.compact.BinaryPowerProduct
import scas.base.ModInteger

trait BinaryPolynomial[T] extends ConvertablePolynomial[T, Int, Int] with MutablePolynomial[T, Int, Array[Int]] {
  override given ring: ModInteger = ModInteger("2")
  given pp: BinaryPowerProduct = deferred
}
