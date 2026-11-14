package scas.adapter.math3

import java.lang.Math
import scas.structure.commutative.Field
import scas.base.BigInteger

type Double = scala.Double

object Double extends Double.Impl with Field.Conv[Double] {
  given instance: Double.type = this
  abstract class Impl extends Field[Double] {
    override def random(numbits: Int)(using rnd: java.util.Random) = rnd.nextDouble()
    def fromInt(n: BigInteger) = n.doubleValue()
    override val zero = 0
    override val one = 1
    extension (x: Double) {
      def add(y: Double) = x + y
      def subtract(y: Double) = x - y
      def multiply(y: Double) = x * y
      override def divide(y: Double) = x / y
      override def isSquare = x >= 0
      def isPower(n: Int) = if n % 2 == 0 then x.isSquare else true
      def nthrt(n: Int) = if n == 2 then sqrt(x) else Math.pow(x, n)
    }
    override def sqrt(x: Double) = Math.sqrt(x)
    def inverse(x: Double) = 1 / x
    def equiv(x: Double, y: Double) = x == y
    extension (x: Double) def signum = Math.signum(x).toInt
    def characteristic = BigInteger("0")
    extension (x: Double) {
      def toCode(level: Level) = x.toString
      def toMathML = s"<cn>$x</cn>"
    }
    override def toString = "Double"
    def toMathML = "<reals/>"
  }
}
