package scas.base

import scas.structure.commutative.ordered.EuclidianDomain

type BigInteger = java.math.BigInteger

object BigInteger extends BigInteger.Impl with EuclidianDomain[BigInteger] {
  given instance: BigInteger.type = this
  abstract class Impl extends EuclidianDomain.Impl[BigInteger] {
    given instance: Impl
    val self: Impl = this
    def apply(n: Long) = java.math.BigInteger.valueOf(n)
    def apply(str: String) = new BigInteger(str)
    extension (x: BigInteger) {
      def add(y: BigInteger) = x.add(y)
      def subtract(y: BigInteger) = x.subtract(y)
      def multiply(y: BigInteger) = x.multiply(y)
    }
    def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
    override def gcd(x: BigInteger, y: BigInteger) = x.gcd(y)
    extension (x: BigInteger) {
      override def divide(y: BigInteger) = x.divide(y)
      override def remainder(y: BigInteger) = x.remainder(y)
      def divideAndRemainder(y: BigInteger) = {
        val Array(q, r) = x.divideAndRemainder(y)
        (q, r)
      }
    }
    lazy val characteristic = BigInteger("0")
    extension (x: BigInteger) def isUnit = abs(x).isOne
    extension (x: BigInteger) override def isZero = x.signum == 0
    extension (a: BigInteger) override def pow(b: BigInteger) = a.pow(b.intValue)
    extension (x: BigInteger) override def negate = x.negate
    override def abs(x: BigInteger) = x.abs
    extension (x: BigInteger) override def signum = x.signum
    extension (x: BigInteger) def toCode(level: Level) = {
      if (x.bitLength < 32) x.toString
      else if (x.bitLength < 64) x.toString + "l"
      else s"BigInteger(\"$x\")"
    }
    override def toString = "BigInteger"
    extension (x: BigInteger) def toMathML = s"<cn>$x</cn>"
    def toMathML = "<integers/>"
    lazy val zero = BigInteger("0")
    lazy val one = BigInteger("1")

    given int2bigInt: (Int => BigInteger) = this(_)
    given long2bigInt: (Long => BigInteger) = this(_)
  }
}
