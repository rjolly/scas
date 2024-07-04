package scas.module.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.structure.conversion.AbelianGroup

class ArrayModule[R : ClassTag](ring: Ring[R], dimension: Int) extends scas.module.ArrayModule[R](ring, dimension) with AbelianGroup[Array[R]] {
  given instance: ArrayModule[R] = this
}
