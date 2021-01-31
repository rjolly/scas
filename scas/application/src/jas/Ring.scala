package jas

import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory

class Ring[T <: RingElem[T] : RingFactory] extends scas.structure.ordered.Ring[T] {
  def factory = summon[RingFactory[T]]
  extension (x: T) {
    def add(y: T) = x.sum(y)
    def subtract(y: T) = x.subtract(y)
    def * (y: T) = x.multiply(y)
  }
  def compare(x: T, y: T) = x.compareTo(y)
  extension (x: T) def isUnit = x.isUnit
  def characteristic = factory.characteristic
  def zero = factory.getZERO()
  def one = factory.getONE()
  extension (x: T) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???
}
