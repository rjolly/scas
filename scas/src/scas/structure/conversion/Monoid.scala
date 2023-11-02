package scas.structure.conversion

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger

trait Monoid[T] extends scas.structure.Monoid[T] with SemiGroup[T] {
  extension (a: T) {
    def \ [U: Conversion[BigInteger]](b: U) = a.pow(~b)
    def \:[U: Conversion[BigInteger]](b: U) = a \ b
  }
  extension[U: Conversion[T]] (a: U) {
    def \ [V: Conversion[BigInteger]](b: V) = (~a).pow(~b)
  }
  extension[U: Conversion[T], V: Conversion[BigInteger]] (a: U) {
    def \:(b: V) = a \ b
  }
}
