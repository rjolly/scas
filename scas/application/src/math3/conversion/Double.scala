package math3.conversion

import scas.structure.conversion.Field
import math3.Double.Impl

object Double extends Impl with Field[Double] {
  given Double.type = this
}
