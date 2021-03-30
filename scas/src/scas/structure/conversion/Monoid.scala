package scas.structure.conversion

import scas.base.BigInteger

trait Monoid[T] extends scas.structure.Monoid[T] with SemiGroup[T] {
  extension[U] (a: U)(using c: U => T) {
    def \ (b: BigInteger): T = c(a).pow(b)
    def \:(b: BigInteger) = a \ b
  }
  extension (a: T) {
    def \ [U](b: U)(using c: U => BigInteger): T = a.pow(c(b))
    def \:[U](b: U)(using c: U => BigInteger) = a \ b
  }
}
