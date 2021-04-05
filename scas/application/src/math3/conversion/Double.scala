package math3.conversion

import scas.structure.conversion.Field
import scas.base.conversion.BigInteger
import math3.Double.Impl
import BigInteger.given

object Double extends Impl with Field[Double] {
  given Double.type = this
  def characteristic = BigInteger(0)
}
