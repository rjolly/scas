package scas.module

import scas.util.{ToFrags, unary_~}

object Array {
  def apply[R, S : ToFrags[R]](using factory: ArrayModule[R])(x: S) = factory(~x)
  def unapplySeq[T](x: Array[T]) = scala.Array.unapplySeq(x)
}
