package scas

import scas.structure.Ring

package object arith with
  type BigInteger = java.math.BigInteger

  given BigInteger as Ring[BigInteger] with
    def apply(n: Long) = java.math.BigInteger.valueOf(n)
    def (x: BigInteger) + (y: BigInteger) = x.add(y)
    def (x: BigInteger) - (y: BigInteger) = x.subtract(y)
    def (x: BigInteger) * (y: BigInteger) = x.multiply(y)
    def (x: BigInteger) isZero = x == zero
    def (x: BigInteger) isOne = x == one
    def zero = BigInteger(0)
    def one = BigInteger(1)
