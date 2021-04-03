package scas.structure.conversion

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger

trait Monoid[T] extends scas.structure.Monoid[T] with SemiGroup[T] {
  extension[U: Conversion[T]] (a: U) {
    def \ (b: BigInteger): T = (~a).pow(b)
    def \:(b: BigInteger) = a \ b
  }
  extension (a: T) {
    def \ [U: Conversion[BigInteger]](b: U): T = a.pow(~b)
    def \:[U: Conversion[BigInteger]](b: U) = a \ b
  }
}
