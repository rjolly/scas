package scas.module.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.structure.conversion.Module
import scas.util.{ToFrags, ClassTagArray}

class ArrayModule[R : ClassTag : ClassTagArray](using Ring[R])(val dimension: Int) extends Module[Array[R], R] with scas.module.ArrayModule[R] {
  given ArrayModule[R] = this
  def apply[S : ToFrags[R]](x: S): Array[R] = super.apply(x.toFrags)
}
