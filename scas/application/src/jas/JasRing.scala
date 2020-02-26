package jas

import scas.prettyprint.Level
import scas.structure.ordered.Ring
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory

class JasRing[T <: RingElem[T] : RingFactory] extends Ring[T] with
  def (x: T) + (y: T) = x.sum(y)
  def (x: T) - (y: T) = x.subtract(y)
  def (x: T) * (y: T) = x.multiply(y)
  def compare(x: T, y: T) = x.compareTo(y)
  def (x: T).isUnit = x.isUnit
  def characteristic = RingFactory[T].characteristic
  def zero = RingFactory[T].getZERO()
  def one = RingFactory[T].getONE()
  def (x: T).toCode(level: Level) = x.toString
  def (x: T).toMathML: String = ???

object RingFactory with
  def apply[T <: RingElem[T] : RingFactory] = summon[RingFactory[T]]
