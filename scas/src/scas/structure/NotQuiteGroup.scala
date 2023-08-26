package scas.structure

import scas.util.{Conversion, unary_~}

trait NotQuiteGroup[T] extends impl.NotQuiteGroup[T] with Monoid[T] {
  abstract override def inverse(x: T) = super.inverse(x)
  def inverse[U: Conversion[T]](x: U): T = inverse(~x)
}
