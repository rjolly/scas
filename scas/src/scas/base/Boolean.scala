package scas.base

import scas.structure.BooleanRing
import scas.structure.commutative.Field

object Boolean extends Boolean.Impl with scas.structure.commutative.conversion.Field[Boolean] with scas.structure.conversion.BooleanRing[Boolean] {
  given instance: Boolean.type = this
  abstract class Impl extends Field[Boolean] with BooleanRing[Boolean] {
    def fromInt(n: BigInteger) = n.signum() != 0
    extension (x: Boolean) {
      def add(y: Boolean) = x ^ y
      def subtract(y: Boolean) = x + y
      def multiply(y: Boolean) = x && y
    }
    def inverse(x: Boolean) = x
    def equiv(x: Boolean, y: Boolean) = x == y
    extension (x: Boolean) def signum = if (x) 1 else 0
    extension (x: Boolean) def toCode(level: Level) = x.toString
    override def toString = "Boolean"
    extension (x: Boolean) def toMathML = if (x) "<true/>" else "<false/>"
    def toMathML = "<ci>Boolean</ci>"
  }
  val characteristic = BigInteger("2")
  val zero = false
  val one = true
}
