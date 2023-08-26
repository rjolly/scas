package scas.module

import scala.reflect.ClassTag
import scas.structure.impl.Ring
import scas.structure.Module
import scas.util.ClassTagArray

class ArrayModule[R : ClassTag : ClassTagArray](using Ring[R])(dimension: Int) extends impl.ArrayModule[R](dimension) with Module[Array[R], R] {
  given instance: ArrayModule[R] = this
}
