package rings

import cc.redberry.rings.io.Coder
import scas.util.unary_~
import BigInteger.given

trait Ring[T] extends Ring.Impl[T] with scas.structure.ordered.Ring[T]

object Ring {
  trait Impl[T] extends scas.structure.ordered.Ring.Impl[T] {
    def ring: cc.redberry.rings.Ring[T]
    def coder = Coder.mkCoder(ring)
    def apply(n: Long) = ring.valueOf(n)
    extension (x: T) {
      def add(y: T) = ring.add(x, y)
      def subtract(y: T) = ring.subtract(x, y)
      def multiply(y: T) = ring.multiply(x, y)
    }
    def compare(x: T, y: T) = ring.compare(x, y)
    extension (x: T) def isUnit = ring.isUnit(x)
    def characteristic = ~ring.characteristic
    def zero = ring.getZero()
    def one = ring.getOne()
    extension (x: T) {
      def toCode(level: Level) = coder.stringify(x)
      def toMathML = ???
    }
    def toMathML = ???
  }
  def apply[T : Ring.Impl] = summon[Ring.Impl[T]]
}
