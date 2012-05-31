package scas.structure

import scas.Implicits.infixUFDOps

trait Residue[T, R] extends UniqueFactorizationDomain[T] {
  implicit val ring: UniqueFactorizationDomain[R]
  val self = this
  def convert(x: T) = {
    val self(a) = x
    apply(ring.convert(a))
  }
  def apply(l: Long) = apply(ring(l))
  def apply(value: R): T
  def reduce(value: R): T
  def unapply(x: T): Option[R]
  def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(ring.random(numbits))
  def isUnit(x: T) = {
    val self(a) = x
    a.isUnit
  }
  override def pow(x: T, exp: java.math.BigInteger) = {
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
  override def signum(x: T) = {
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
    apply(ring.gcd(a, b))
  }
  def divideAndRemainder(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    val (q, r) = a /% b
    (apply(q), apply(r))
  }
  def compare(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    ring.compare(a, b)
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
