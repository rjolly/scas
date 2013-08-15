package scas.structure

import scas.BigInteger
import scas.Implicits.infixUFDOps

trait Residue[@specialized(Int, Long) T, @specialized(Int, Long) R] extends UniqueFactorizationDomain[T] { self =>
  implicit val ring: UniqueFactorizationDomain[R]
  override def convert(x: T) = {
    val self(a) = x
    reduce(ring.convert(a))
  }
  def apply(l: Long) = reduce(ring(l))
  def fromRing(value: R): T
  def reduce(value: R): T
  def unapply(x: T): Option[R]
  def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(ring.random(numbits))
  def isUnit(x: T) = {
    val self(a) = x
    a.isUnit
  }
  override def pow(x: T, exp: BigInteger) = {
    val self(a) = x
    reduce(ring.pow(a, exp))
  }
  override def negate(x: T) = {
    val self(a) = x
    reduce(-a)
  }
  override def abs(x: T) = {
    val self(a) = x
    reduce(ring.abs(a))
  }
  def signum(x: T) = {
    val self(a) = x
    ring.signum(a)
  }
  def plus(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    reduce(a + b)
  }
  def minus(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    reduce(a - b)
  }
  def times(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    reduce(a * b)
  }
  def gcd(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    fromRing(ring.gcd(a, b))
  }
  def divideAndRemainder(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    val (q, r) = a /% b
    (fromRing(q), fromRing(r))
  }
  def equiv(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    a >< b
  }
  override def toCode(x: T, precedence: Int) = {
    val self(a) = x
    a.toCode(precedence)
  }
  def toMathML(x: T) = {
    val self(a) = x
    a.toMathML
  }
}

object Residue {
  trait Element[T <: Element[T, R], R] extends UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: Residue[T, R]
    val value: R
  }
}
