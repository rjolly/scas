package scas.structure

import scas.{BigInteger, int2bigInt}
import BigInteger.given

trait Monoid[T] extends SemiGroup[T] {
  extension (a: T) {
    def \[U](b: U)(using c: U => BigInteger): T = a \ c(b)
    def \ (b: BigInteger): T = {
      assert (b.signum >= 0)
      if (b.isZero) one else if ((b % 2).isZero) {
        val c = a \ (b / 2)
        c * c
      } else {
        a * a \ (b - 1)
      }
    }
    def \:(b: BigInteger) = a \ b
  }
  extension (x: T) def isUnit: Boolean
  def one: T
  extension (x: T) def isOne = x >< one
}
