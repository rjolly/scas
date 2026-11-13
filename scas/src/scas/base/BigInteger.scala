package scas.base

import scala.compiletime.deferred
import scas.factors.IntegerFactors
import scas.structure.commutative.ordered.{UniqueFactorizationDomain, EuclidianDomain}
import java.math.BigInteger.valueOf

type BigInteger = java.math.BigInteger

object BigInteger extends BigInteger.Impl with UniqueFactorizationDomain.Conv[BigInteger] {
  override given instance: BigInteger.type = this
  given factors: IntegerFactors.type = IntegerFactors
  trait Impl extends EuclidianDomain[BigInteger] {
    given instance: BigInteger.Impl = deferred
    val self = this
    def fromInt(n: BigInteger) = n
    override val zero = this("0")
    override val one = this("1")
    def apply(str: String): BigInteger = new BigInteger(str)
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
      override def isSquare = if x.signum < 0 then false else {
        val Array(_, r) = x.sqrtAndRemainder()
        r.isZero
      }
      def isPower(n: Int) = if n == 2 then x.isSquare else {
        if x.isZero then true else if x.signum < 0 then {
          if n % 2 == 0 then false else (-x).isPower(n)
        } else factors(x, false).isPower(n)
      }
      def nthrt(n: Int) = if n == 2 then sqrt(x) else {
        if x.isZero then zero else if x.signum < 0 then {
          if n % 2 == 0 then ??? else -(-x).nthrt(n)
        } else factors(x, false).nthrt(n).expand
      }
    }
    def factorize(x: BigInteger) = factors(x)
    override def sqrt(x: BigInteger) = x.sqrt()
    def characteristic = zero
    extension (x: BigInteger) def isUnit = abs(x).isOne
    extension (x: BigInteger) override def isZero = x.signum == 0
    extension (a: BigInteger) override def pow(b: BigInteger) = a.pow(b.intValue)
    extension (x: BigInteger) override def unary_- = x.negate
    override def abs(x: BigInteger) = x.abs
    extension (x: BigInteger) override def signum = x.signum
    extension (x: BigInteger) def toCode(level: Level) = {
      if x.bitLength < 32 then x.toString
      else if x.bitLength < 64 then x.toString + "l"
      else s"BigInteger(\"$x\")"
    }
    override def toString = "BigInteger"
    extension (x: BigInteger) def toMathML = s"<cn>$x</cn>"
    def toMathML = "<integers/>"

    given int2bigInt: (Int => BigInteger) = valueOf(_)
    given long2bigInt: (Long => BigInteger) = valueOf(_)
  }
}
