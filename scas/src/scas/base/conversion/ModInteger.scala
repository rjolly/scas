package scas.base.conversion

import scas.structure.commutative.ordered.conversion.Field
import scas.util.{Conversion, unary_~}
import BigInteger.int2bigInt

class ModInteger(mod: BigInteger) extends scas.base.ModInteger(mod) with Field[BigInteger] {
  given ModInteger = this

  extension (ring: scas.base.BigInteger.Impl) def residue(n: Int) = super.residue(ring)(BigInteger(n))
}

object ModInteger {
  def apply[U: Conversion[BigInteger]](x: U) = new ModInteger(~x)
}
