package scas.base.conversion

import scas.structure.commutative.UniqueFactorizationDomain
import scas.structure.commutative.ordered.conversion.Field
import scas.util.{Conversion, unary_~}
import scas.base.ModInteger.Impl
import BigInteger.int2bigInt

class ModInteger(mod: BigInteger) extends scas.base.ModInteger(mod) with Field[BigInteger] {
  given ModInteger = this
}

object ModInteger extends Impl {
  def apply[U: Conversion[BigInteger]](x: U) = new ModInteger(~x)

  override def apply(str: String) = super.apply(str)
}
