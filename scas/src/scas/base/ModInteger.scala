package scas.base

import scas.structure.commutative.ordered.{Residue, Field}
import BigInteger.given

class ModInteger(val mod: BigInteger) extends impl.ModInteger with Residue[BigInteger] with Field[BigInteger] {
  given instance: ModInteger = this
}

object ModInteger {
  def apply(str: String) = new ModInteger(BigInteger(str))
}
