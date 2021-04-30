package scas.base.conversion

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.conversion.Field

class ModInteger(mod: BigInteger) extends scas.base.ModInteger(mod) with Field[BigInteger] {
  given ModInteger = this
}

object ModInteger {
  def apply(str: String) = new ModInteger(BigInteger(str))
}
