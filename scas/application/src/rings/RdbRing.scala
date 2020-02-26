package rings

import scas.prettyprint.Level
import cc.redberry.rings.Ring

class RdbRing[T : Ring] extends scas.structure.ordered.Ring[T] with
  def (x: T) + (y: T) = Ring[T].add(x, y)
  def (x: T) - (y: T) = Ring[T].subtract(x, y)
  def (x: T) * (y: T) = Ring[T].multiply(x, y)
  def compare(x: T, y: T) = Ring[T].compare(x, y)
  def (x: T).isUnit = Ring[T].isUnit(x)
  def characteristic = Ring[T].characteristic
  def zero = Ring[T].getZero()
  def one = Ring[T].getOne()
  def (x: T).toCode(level: Level) = x.toString
  def (x: T).toMathML: String = ???

object Ring with
  def apply[T : Ring] = summon[Ring[T]]
