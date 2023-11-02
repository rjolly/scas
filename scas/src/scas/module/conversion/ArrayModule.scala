package scas.module.conversion

import scala.reflect.ClassTag
import scas.structure.impl.Ring
import scas.structure.conversion.Module

class ArrayModule[R : ClassTag](using Ring[R])(dimension: Int) extends scas.module.ArrayModule[R](dimension) with Module[Array[R], R] {
  given instance: ArrayModule[R] = this
}
