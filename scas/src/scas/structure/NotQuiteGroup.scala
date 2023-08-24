package scas.structure

import scas.base.BigInteger
import scas.util.{Conversion, unary_~}
import BigInteger.self.given

trait NotQuiteGroup[T] extends NotQuiteGroup.Impl[T] with Monoid[T] {
  abstract override def inverse(x: T) = super.inverse(x)
  def inverse[U: Conversion[T]](x: U): T = inverse(~x)
}

object NotQuiteGroup {
  trait Impl[T] extends Monoid.Impl[T] {
    extension (a: T) override def pow(b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else super.pow(a)(b)
    def inverse(x: T): T
  }
}
