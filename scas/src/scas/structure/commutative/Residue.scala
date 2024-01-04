package scas.structure.commutative

import scas.base.BigInteger

trait Residue[T, R](using ring: UniqueFactorizationDomain[R]) extends UniqueFactorizationDomain[T] {
  def apply(x: R): T
  val self = this
  def unapply(x: T): R
  def fromInt(n: BigInteger) = this(ring.fromInt(n))
  def fromRing(x: R): T
  extension (x: T) {
    def signum = {
      val self(a) = x
      ring.signum(a)
    }
    def add(y: T) = {
      val self(a) = x
      val self(b) = y
      this(a + b)
    }
    def subtract(y: T) = {
      val self(a) = x
      val self(b) = y
      this(a - b)
    }
    def multiply(y: T) = {
      val self(a) = x
      val self(b) = y
      this(a * b)
    }
  }
  def equiv(x: T, y: T) = {
    val self(a) = x
    val self(b) = y
    val self(c) = this(a)
    val self(d) = this(b)
    c >< d
  }
  extension (x: T) {
    def toCode(level: Level) = {
      val self(a) = x
      a.toCode(level)
    }
    def toMathML = {
      val self(a) = x
      a.toMathML
    }
  }
  val zero = fromRing(ring.zero)
  val one = fromRing(ring.one)

  extension (ring: UniqueFactorizationDomain[R]) def apply(s: R*): Residue[T, R]
}
