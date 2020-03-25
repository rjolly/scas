package scas.structure

import scas.{BigInteger, int2bigInt}

trait Monoid[T] extends SemiGroup[T] {
  def (a: T) \ (b: BigInteger): T = {
    assert (b >= 0)
    if (b >< 0) one else if (b % 2 >< 0) {
      val c = a \ (b / 2)
      c * c
    } else {
      a * a \ (b - 1)
    }
  }
  def (a: T) \:(b: BigInteger) = a \ b
  def (x: T).isUnit: Boolean
  def one: T
}
