package rings

import cc.redberry.rings.Ring

class RdbRing[T : Ring] extends scas.structure.ordered.Ring[T] {
  def ring = summon[Ring[T]]
  def (x: T) + (y: T) = ring.add(x, y)
  def (x: T) - (y: T) = ring.subtract(x, y)
  def (x: T) * (y: T) = ring.multiply(x, y)
  def compare(x: T, y: T) = ring.compare(x, y)
  def (x: T).isUnit = ring.isUnit(x)
  def characteristic = ring.characteristic
  def zero = ring.getZero()
  def one = ring.getOne()
  def (x: T).toCode(level: Level) = x.toString
  def (x: T).toMathML: String = ???
}
