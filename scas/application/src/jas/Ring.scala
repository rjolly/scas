package jas

import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory

class Ring[T <: RingElem[T] : RingFactory] extends scas.structure.ordered.Ring[T] {
  def factory = summon[RingFactory[T]]
  def (x: T) + (y: T) = x.sum(y)
  def (x: T) - (y: T) = x.subtract(y)
  def (x: T) * (y: T) = x.multiply(y)
  def compare(x: T, y: T) = x.compareTo(y)
  def (x: T).isUnit = x.isUnit
  def characteristic = factory.characteristic
  def zero = factory.getZERO()
  def one = factory.getONE()
  def (x: T).toCode(level: Level) = x.toString
  def (x: T).toMathML = ???
  def toMathML = ???
}