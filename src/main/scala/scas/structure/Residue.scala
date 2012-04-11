package scas.structure

import scas.Implicits.infixUFDOps

abstract class Residue[T, R](implicit val ring: UniqueFactorizationDomain[R]) extends Ring[T] {
  def apply(x: T) = fromRing(toRing(x))
  def apply(l: Long) = fromRing(ring(l))
  def fromRing(x: R): T
  def toRing(x: T): R
  def random(numbits: Int)(implicit rnd: java.util.Random) = fromRing(ring.random(numbits))
  def isUnit(x: T) = toRing(x).isUnit
  override def pow(x: T, exp: java.math.BigInteger) = fromRing(ring.pow(toRing(x), exp))
  override def negate(x: T) = fromRing(-toRing(x))
  override def abs(x: T) = x
  override def signum(x: T) = ring.signum(toRing(x))
  def plus(x: T, y: T) = fromRing(toRing(x) + toRing(y))
  def minus(x: T, y: T) = fromRing(toRing(x) - toRing(y))
  def times(x: T, y: T) = fromRing(toRing(x) * toRing(y))
  def compare(x: T, y: T) = ring.compare(toRing(x), toRing(y))
  override def toCode(x: T, precedence: Int) = toRing(x).toCode(precedence)
}

object Residue {
  abstract class Element[T <: Element[T, R], R](val value: R)(val factory: Residue[T, R]) extends Ring.Element[T] { this: T =>
  }
}
