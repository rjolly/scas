package scas.adapter.jas

import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory
import scas.base.BigInteger

trait Ring[T <: RingElem[T]] extends scas.structure.ordered.Ring[T] {
  def factory: RingFactory[T]
  def fromInt(n: BigInteger) = factory.fromInteger(n)
  override def zero = factory.getZERO()
  override def one = factory.getONE()
  extension (x: T) {
    def add(y: T) = x.sum(y)
    def subtract(y: T) = x.subtract(y)
    def multiply(y: T) = x.multiply(y)
  }
  def compare(x: T, y: T) = x.compareTo(y)
  extension (x: T) def isUnit = x.isUnit
  def characteristic = factory.characteristic
  extension (x: T) {
    def toCode(level: Level) = x.toString
    def toMathML = ???
  }
  def toMathML = ???
}

object Ring {
  trait Conv[T <: RingElem[T]] extends Ring[T] with scas.structure.ordered.Ring.Conv[T]
}
