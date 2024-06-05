package scas.module.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.structure.conversion.AbelianGroup

class ArrayModule[R : ClassTag](using Ring[R])(dimension: Int) extends scas.module.ArrayModule[R](dimension) with AbelianGroup[Array[R]] {
  given instance: ArrayModule[R] = this
}
