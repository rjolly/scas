package rings

import cc.redberry.rings.Ring
import cc.redberry.rings.io.Coder

abstract class RdbRing[T] extends scas.structure.ordered.Ring[T] {
  def ring: Ring[T]
  def coder = Coder.mkCoder(ring)
  def (x: T) + (y: T) = ring.add(x, y)
  def (x: T) - (y: T) = ring.subtract(x, y)
  def (x: T) * (y: T) = ring.multiply(x, y)
  def compare(x: T, y: T) = ring.compare(x, y)
  def (x: T).isUnit = ring.isUnit(x)
  def characteristic = ring.characteristic
  def zero = ring.getZero()
  def one = ring.getOne()
  def (x: T).toCode(level: Level) = coder.stringify(x)
  def (x: T).toMathML = ???
  def toMathML = ???
}

object RdbRing {
  def apply[T : RdbRing] = summon[RdbRing[T]]
}
