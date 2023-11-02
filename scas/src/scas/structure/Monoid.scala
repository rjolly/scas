package scas.structure

import scas.base.BigInteger
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
    def \ (b: BigInteger) = a.pow(b)
    def \:(b: BigInteger) = a \ b
  }
  extension (x: T) {
    def isUnit: Boolean
    def isOne = x >< one
  }
  def one: T
}
