package scas.module

import scas.util.ToFrags
import scas.structure.Ring

type ClassTag[N] = scala.reflect.ClassTag[N]
type ClassTagArray[N] = ClassTag[Array[N]]

object Array {
  def apply[R, S : ToFrags[R]](using factory: ArrayModule[R])(x: S) = factory(x)
  def unapplySeq[T](x: Array[T]) = scala.Array.unapplySeq(x)
}
