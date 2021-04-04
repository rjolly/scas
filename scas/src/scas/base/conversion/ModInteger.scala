package scas.base.conversion

import scas.base.BigInteger
import scas.structure.commutative.ordered.conversion.Field

class ModInteger(mod: BigInteger) extends scas.base.ModInteger(mod) with Field[BigInteger] {
  given ModInteger = this
}

object ModInteger {
  def apply[U](x: U)(using c: U => BigInteger) = new ModInteger(c(x))
}
