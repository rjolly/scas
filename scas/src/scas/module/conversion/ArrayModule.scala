package scas.module.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.structure.conversion.Module
import scas.util.ClassTagArray

class ArrayModule[R : ClassTag : ClassTagArray](using Ring[R])(dimension: Int) extends scas.module.ArrayModule[R](using Ring[R])(dimension) with Module[Array[R], R] {
  given ArrayModule[R] = this
}
