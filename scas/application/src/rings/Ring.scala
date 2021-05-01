package rings

import cc.redberry.rings.io.Coder
import scas.util.unary_~
import BigInteger.given

trait Ring[T] extends scas.structure.ordered.conversion.Ring[T] {
  def ring: cc.redberry.rings.Ring[T]
  def coder = Coder.mkCoder(ring)
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

object Ring {
  def apply[T : Ring] = summon[Ring[T]]
}
