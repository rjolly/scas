package scas.module.conversion

import scas.util.ToFrags

object Array {
  def apply[R, S : ToFrags[R]](using factory: ArrayModule[R])(x: S) = factory(x)
  def unapplySeq[T](x: Array[T]) = scala.Array.unapplySeq(x)
}
