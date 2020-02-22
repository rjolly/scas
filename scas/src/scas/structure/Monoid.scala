package scas.structure

import scas.\:

trait Monoid[T] extends SemiGroup[T] with
  given Monoid[T] = this
  def (a: T) \ (n: Long): T = if (n == 0) one else if (n % 2 == 0)
    val b = a \: (n / 2)
    b * b
  else
    a * a \: (n - 1)
  def (x: T) isOne: Boolean
  def one: T
