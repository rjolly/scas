package scas.structure

import scas.util.{Conversion, unary_~}
import scas.base.conversion.BigInteger
import BigInteger.given

trait Monoid[T] extends SemiGroup[T] {
  extension (a: T) {
    def pow(b: BigInteger): T = {
      assert (b.signum >= 0)
      if (b.isZero) one else if ((b % 2).isZero) {
        val c = a \ (b / 2)
        c * c
      } else {
        a * a \ (b - 1)
      }
    }
    def \ [U: Conversion[BigInteger]](b: U) = a.pow(~b)
    def \:[U: Conversion[BigInteger]](b: U) = a \ b
  }
  extension[U: Conversion[T]] (a: U) {
    def \ [V: Conversion[BigInteger]](b: V) = (~a).pow(~b)
  }
  extension[U: Conversion[T], V: Conversion[BigInteger]] (a: U) {
    def \:(b: V) = a \ b
  }
  extension (x: T) {
    def isUnit: Boolean
    def isOne = x >< one
  }
  def one: T
}
