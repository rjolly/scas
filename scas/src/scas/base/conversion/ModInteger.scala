package scas.base.conversion

import scas.base.BigInteger
import scas.structure.commutative.ordered.conversion.Field
import scas.util.{Conversion, unary_~}

class ModInteger(mod: BigInteger) extends scas.base.ModInteger(mod) with Field[BigInteger] {
  given ModInteger = this
}

object ModInteger {
  def apply[U: Conversion[BigInteger]](x: U) = new ModInteger(~x)
}
