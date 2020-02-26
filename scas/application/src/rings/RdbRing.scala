package rings

import scas.prettyprint.Level
import cc.redberry.rings.Ring

class RdbRing[T : Ring] extends scas.structure.ordered.Ring[T] with
  def (x: T) + (y: T) = summon[Ring[T]].add(x, y)
  def (x: T) - (y: T) = summon[Ring[T]].subtract(x, y)
  def (x: T) * (y: T) = summon[Ring[T]].multiply(x, y)
  def compare(x: T, y: T) = summon[Ring[T]].compare(x, y)
  def (x: T).isUnit = summon[Ring[T]].isUnit(x)
  def characteristic = summon[Ring[T]].characteristic
  def zero = summon[Ring[T]].getZero()
  def one = summon[Ring[T]].getOne()
  def (x: T).toCode(level: Level) = x.toString
  def (x: T).toMathML: String = ???
