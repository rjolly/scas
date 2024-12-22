package scas.base

import scala.compiletime.deferred
import scas.structure.BooleanRing
import scas.structure.commutative.Field

type Boolean = scala.Boolean

object Boolean extends Boolean.Impl with Field.Conv[Boolean] with BooleanRing.Conv[Boolean] {
  override given instance: Boolean.type = this
  trait Impl extends Field[Boolean] with BooleanRing[Boolean] {
    given instance: Boolean.Impl = deferred
    val self = this
    def fromInt(n: BigInteger) = n.signum != 0
    override val zero = false
    override val one = true
    extension (x: Boolean) {
      def add(y: Boolean) = x ^ y
      def subtract(y: Boolean) = x + y
      def multiply(y: Boolean) = x && y
    }
    def inverse(x: Boolean) = x
    val characteristic = BigInteger("2")
    def equiv(x: Boolean, y: Boolean) = x == y
    extension (x: Boolean) def signum = if (x) 1 else 0
    extension (x: Boolean) def toCode(level: Level) = x.toString
    override def toString = "Boolean"
    extension (x: Boolean) def toMathML = if (x) "<true/>" else "<false/>"
    def toMathML = "<ci>Boolean</ci>"
  }
}
