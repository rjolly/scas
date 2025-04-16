package scas.module

import scas.util.{ToFrags, unary_~}

object Array {
  def apply[R : ArrayModule as factory, S : ToFrags[R]](x: S) = factory(~x)
  def unapplySeq[T](x: Array[T]) = scala.Array.unapplySeq(x)
}
