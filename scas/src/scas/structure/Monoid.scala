package scas.structure

import scas.{BigInteger, int2bigInt}

trait Monoid[T] extends SemiGroup[T] with
  def (a: T) \ (n: BigInteger) = {
    assert (n >= 0)
    if (n >< 0) one else if (n % 2 >< 0)
      val b = a \:(n / 2)
      b * b
    else
      a * a \:(n - 1)
  }
  def (a: T) \:(n: BigInteger): T = a \ n
  def (x: T).isUnit: Boolean
  def one: T
