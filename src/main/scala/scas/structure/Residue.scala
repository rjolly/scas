package scas.structure

import scas.Implicits.infixUFDOps

trait Residue[T, R] extends Ring[T] {
  implicit val ring: UniqueFactorizationDomain[R]
  def convert(x: T) = fromRing(toRing(x))
  def apply(l: Long) = fromRing(ring(l))
  def fromRing(x: R): T
  def toRing(x: T): R
  def random(numbits: Int)(implicit rnd: java.util.Random) = fromRing(ring.random(numbits))
  def isUnit(x: T) = toRing(x).isUnit
  override def pow(x: T, exp: java.math.BigInteger) = fromRing(ring.pow(toRing(x), exp))
  override def negate(x: T) = fromRing(-toRing(x))
  override def abs(x: T) = fromRing(ring.abs(toRing(x)))
  override def signum(x: T) = ring.signum(toRing(x))
  def plus(x: T, y: T) = fromRing(toRing(x) + toRing(y))
  def minus(x: T, y: T) = fromRing(toRing(x) - toRing(y))
  def times(x: T, y: T) = fromRing(toRing(x) * toRing(y))
  def compare(x: T, y: T) = ring.compare(toRing(x), toRing(y))
  override def toCode(x: T, precedence: Int) = toRing(x).toCode(precedence)
  def toMathML(x: T) = toRing(x).toMathML
}

object Residue {
  trait Element[T <: Element[T, R], R] extends Ring.Element[T] { this: T =>
    val factory: Residue[T, R]
    val value: R
  }
}
