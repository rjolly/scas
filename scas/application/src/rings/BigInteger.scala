package rings

import cc.redberry.rings.Rings

type BigInteger = cc.redberry.rings.bigint.BigInteger

object BigInteger {
  abstract class Impl extends Ring[BigInteger] {
    def apply(str: String) = new BigInteger(str)
    val ring = Rings.Z
  }
}
