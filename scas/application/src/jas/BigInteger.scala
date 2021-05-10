package jas

type BigInteger = edu.jas.arith.BigInteger

object BigInteger {
  class Impl extends Ring[BigInteger] {
    val factory = new BigInteger()
    def apply(str: String) = new BigInteger(str)
  }
}
